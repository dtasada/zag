const std = @import("std");
const pretty = @import("pretty");
const utils = @import("utils");
pub const ast = @import("ast.zig");

const statement_handlers = @import("statement_handlers.zig");
const expression_handlers = @import("expression_handlers.zig");

const Lexer = @import("Lexer");
const TypeParser = @import("TypeParser.zig");

const Self = @This();

const StatementHandler = *const fn (*Self) ParserError!ast.Statement;
const NudHandler = *const fn (*Self) ParserError!ast.Expression;
const LedHandler = *const fn (*Self, *const ast.Expression, BindingPower) ParserError!ast.Expression;

const StatementLookup = std.AutoHashMap(Lexer.TokenKind, StatementHandler);
const NudLookup = std.AutoHashMap(Lexer.TokenKind, NudHandler);
const LedLookup = std.AutoHashMap(Lexer.TokenKind, LedHandler);
const BpLookup = std.AutoHashMap(Lexer.TokenKind, BindingPower);

pub fn hash(context: anytype, key: anytype, depth: u32) void {
    if (depth > 100) { // arbitrary recursion limit
        return;
    }
    const Key = @TypeOf(key);
    switch (@typeInfo(Key)) {
        .noreturn, .type, .undefined, .null, .void => {},
        .comptime_int, .comptime_float => @compileError("unable to hash comptime value"),

        .float => switch (@TypeOf(key)) {
            f32 => { // f32
                const v: u32 = @bitCast(key);
                context.update(std.mem.asBytes(&v));
            },
            f64 => { // f64
                const v: u64 = @bitCast(key);
                context.update(std.mem.asBytes(&v));
            },
            else => context.update(std.mem.asBytes(&key)),
        },

        .int, .bool, .@"enum" => context.update(std.mem.asBytes(&key)),

        .pointer => |info| switch (info.size) {
            .one => {
                const v: usize = @intFromPtr(key);
                context.update(std.mem.asBytes(&v));
            },
            .slice => {
                const len_bytes = std.mem.asBytes(&key.len);
                context.update(len_bytes);
                context.update(std.mem.sliceAsBytes(key));
            },
            else => {
                // Ignore other pointers
            },
        },

        .array => {
            for (key) |item| {
                hash(context, item, depth + 1);
            }
        },

        .@"struct" => |info| {
            inline for (info.fields) |field| {
                if (!field.is_comptime) {
                    hash(context, @field(key, field.name), depth + 1);
                }
            }
        },

        .@"union" => |union_info| {
            hash(context, std.meta.activeTag(key), depth + 1);
            inline for (union_info.fields) |field| {
                if (std.mem.eql(u8, field.name, @tagName(std.meta.activeTag(key)))) {
                    if (field.type != void) {
                        const payload = @field(key, field.name);
                        hash(context, payload, depth + 1);
                    }
                    break;
                }
            }
        },

        .optional => {
            if (key) |payload| {
                context.update(&.{1});
                hash(context, payload, depth + 1);
            } else {
                context.update(&.{0});
            }
        },
        .@"opaque", .@"fn" => @compileError("unable to hash type " ++ @typeName(Key)),
        .error_set => {},
        .frame => @compileError("unable to hash type " ++ @typeName(Key)),
        .@"anyframe" => @compileError("unable to hash type " ++ @typeName(Key)),
        .vector => |info| {
            for (0..info.len) |i| {
                hash(context, key[i], depth + 1);
            }
        },
        .error_union => {},
        .enum_literal => {},
    }
}

/// Binding power. please keep order of enum
pub const BindingPower = enum {
    default,
    comma,
    assignment,
    logical,
    relational,
    additive,
    multiplicative,
    unary,
    call,
    member,
    primary,
};

pub const ParserError = error{
    UnexpectedToken,
    UnexpectedExpression,
    NoSpaceLeft,
    HandlerDoesNotExist,
    OutOfMemory,
    ExpressionNotInMap,
    StatementNotInMap,
};

pos: usize,
lexer: *const Lexer,
source_map: std.AutoHashMap(u64, utils.Position),
alloc: std.mem.Allocator,
output: ast.RootNode = .empty,

/// pratt parsing helpers
type_parser: TypeParser,
bp_lookup: BpLookup,
nud_lookup: NudLookup,
led_lookup: LedLookup,
statement_lookup: StatementLookup,
// errors: std.ArrayList(ParserError) = .empty,

/// Initializes and runs parser. Populates `output`.
pub fn init(input: *const Lexer, alloc: std.mem.Allocator) !*Self {
    const self = try alloc.create(Self);
    self.* = .{
        .pos = 0,
        .lexer = input,
        .bp_lookup = .init(alloc),
        .nud_lookup = .init(alloc),
        .led_lookup = .init(alloc),
        .statement_lookup = .init(alloc),
        .type_parser = try .init(alloc, self),
        .source_map = .init(alloc),
        .alloc = alloc,
    };

    try self.led(Lexer.Token.equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.plus_equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.minus_equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.times_equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.slash_equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.mod_equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.and_equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.or_equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.xor_equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.shift_right_equals, .assignment, expression_handlers.parseAssignmentExpression);
    try self.led(Lexer.Token.shift_left_equals, .assignment, expression_handlers.parseAssignmentExpression);

    // logical
    try self.led(Lexer.Token.@"and", .logical, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.@"or", .logical, expression_handlers.parseBinaryExpression);

    // relational
    try self.led(Lexer.Token.less, .relational, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.less_equals, .relational, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.greater, .relational, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.greater_equals, .relational, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.equals_equals, .relational, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.bang_equals, .relational, expression_handlers.parseBinaryExpression);

    // additive & multiplicative
    try self.led(Lexer.Token.plus, .additive, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.dash, .additive, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.asterisk, .multiplicative, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.slash, .multiplicative, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.percent, .multiplicative, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.ampersand, .multiplicative, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.pipe, .additive, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.caret, .additive, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.pipe, .additive, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.caret, .additive, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.shift_left, .multiplicative, expression_handlers.parseBinaryExpression);
    try self.led(Lexer.Token.shift_right, .multiplicative, expression_handlers.parseBinaryExpression);

    // literals & symbols
    try self.nud(Lexer.Token.int, expression_handlers.parsePrimaryExpression);
    try self.nud(Lexer.Token.float, expression_handlers.parsePrimaryExpression);
    try self.nud(Lexer.Token.ident, expression_handlers.parsePrimaryExpression);
    try self.nud(Lexer.Token.string, expression_handlers.parsePrimaryExpression);
    try self.nud(Lexer.Token.dash, expression_handlers.parsePrefixExpression);
    try self.nud(Lexer.Token.open_paren, expression_handlers.parseGroupExpression);

    // Call/member expressions
    try self.led(Lexer.Token.dot, .member, expression_handlers.parseMemberAccessExpression);
    try self.led(Lexer.Token.open_brace, .call, expression_handlers.parseStructInstantiationExpression);
    try self.led(Lexer.Token.open_paren, .call, expression_handlers.parseCallExpression);
    try self.nud(Lexer.Token.open_bracket, expression_handlers.parseArrayInstantiationExpression);

    // other expressions
    try self.nud(Lexer.Token.open_brace, expression_handlers.parseBlockExpression);
    try self.nud(Lexer.Token.@"if", expression_handlers.parseIfExpression);
    try self.nud(Lexer.Token.ampersand, expression_handlers.parseReferenceExpression);
    try self.led(Lexer.Token.dot_dot, .relational, expression_handlers.parseRangeExpression);
    try self.led(Lexer.Token.dot_dot_equals, .relational, expression_handlers.parseRangeExpression);
    try self.led(Lexer.Token.open_bracket, .call, expression_handlers.parseIndexExpression);

    // Statements
    try self.statement(Lexer.Token.let, statement_handlers.parseVariableDeclarationStatement);
    try self.statement(Lexer.Token.mut, statement_handlers.parseVariableDeclarationStatement);
    try self.statement(Lexer.Token.@"struct", statement_handlers.parseStructDeclarationStatement);
    try self.statement(Lexer.Token.@"enum", statement_handlers.parseEnumDeclarationStatement);
    try self.statement(Lexer.Token.@"union", statement_handlers.parseUnionDeclarationStatement);
    try self.statement(Lexer.Token.@"fn", statement_handlers.parseFunctionDefinition);
    try self.statement(Lexer.Token.@"while", statement_handlers.parseWhileStatement);
    try self.statement(Lexer.Token.@"return", statement_handlers.parseReturnStatement);
    try self.statement(Lexer.Token.@"for", statement_handlers.parseForStatement);
    try self.statement(Lexer.Token.@"if", statement_handlers.parseIfStatement);

    while (std.meta.activeTag(self.currentToken()) != Lexer.Token.eof)
        try self.output.append(self.alloc, try statement_handlers.parseStatement(self));

    return self;
}

pub fn deinit(self: *Self) void {
    self.bp_lookup.deinit();
    self.nud_lookup.deinit();
    self.led_lookup.deinit();
    self.statement_lookup.deinit();
}

pub inline fn currentPosition(self: *const Self) utils.Position {
    const pos = std.math.clamp(self.pos, 0, self.lexer.source_map.items.len - 1);
    return self.lexer.source_map.items[pos];
}

/// Consumes current token and then increases position.
pub inline fn advance(self: *Self) Lexer.Token {
    const current_token = self.lexer.tokens.items[self.pos];
    self.pos += 1;
    return current_token;
}

pub inline fn currentToken(self: *const Self) Lexer.Token {
    return self.lexer.tokens.items[self.pos];
}

pub inline fn currentTokenKind(self: *const Self) Lexer.TokenKind {
    return std.meta.activeTag(self.currentToken());
}

/// A token which has a NUD handler means it expects nothing to its left
/// Common examples of this type of token are prefix & unary expressions.
fn nud(self: *Self, kind: Lexer.TokenKind, nud_fn: NudHandler) !void {
    try self.bp_lookup.put(kind, .primary);
    try self.nud_lookup.put(kind, nud_fn);
}

/// Tokens which have an LED expect to be between or after some other expression
/// to their left. Examples of this type of handler include binary expressions and
/// all infix expressions. Postfix expressions also fall under the LED handler.
fn led(self: *Self, kind: Lexer.TokenKind, bp: BindingPower, led_fn: LedHandler) !void {
    try self.bp_lookup.put(kind, bp);
    try self.led_lookup.put(kind, led_fn);
}

fn statement(self: *Self, kind: Lexer.TokenKind, statment_fn: StatementHandler) !void {
    try self.bp_lookup.put(kind, .default);
    try self.statement_lookup.put(kind, statment_fn);
}

/// Prints error message and always returns an error.
/// `environment` and `expected_token` are strings for the error message.
/// example: "function definition" and "function name" respectively yields:
/// "Unexpected token in function definition '<actual>' at <line>:<col>. Expected 'function_name'",
pub fn unexpectedToken(
    self: *const Self,
    environment: []const u8,
    expected_token: []const u8,
    actual: Lexer.Token,
) error{ NoSpaceLeft, UnexpectedToken } {
    const pos = std.math.clamp(self.pos - 1, 0, self.lexer.source_map.items.len - 1);

    return utils.printErr(
        error.UnexpectedToken,
        "Unexpected token '{f}' in {s} at {f}. Expected '{s}'\n",
        .{
            actual,
            environment,
            self.lexer.source_map.items[pos],
            expected_token,
        },
        .red,
    );
}

/// Returns active tag if active type of `actual` is the same as `expected`. Errors otherwise.
/// `environment` and `expected_token` are strings for the error message.
/// example: "function definition" and "function name" respectively yields
/// "Unexpected token in function definition '<bad_token>' at <line>:<col>. Expected 'function_name'",
pub fn expect(
    self: *const Self,
    actual: Lexer.Token,
    comptime expected: Lexer.TokenKind,
    comptime context: []const u8,
    comptime expected_token: []const u8,
) !@FieldType(Lexer.Token, @tagName(expected)) {
    return if (std.meta.activeTag(actual) == expected)
        @field(actual, @tagName(expected))
    else
        self.unexpectedToken(context, expected_token, actual);
}

/// Identical to `expect` but doesn't print error message.
pub fn expectSilent(
    _: *const Self,
    actual: Lexer.Token,
    comptime expected: Lexer.TokenKind,
) !@FieldType(Lexer.Token, @tagName(expected)) {
    return if (std.meta.activeTag(actual) == expected)
        @field(actual, @tagName(expected))
    else
        error.UnexpectedToken;
}

/// Returns the appropriate handler and errors if a handler isn't found.
pub inline fn getHandler(
    self: *const Self,
    comptime handler_type: enum { statement, nud, led, bp },
    token: Lexer.TokenKind,
) ParserError!switch (handler_type) {
    .statement => StatementHandler,
    .nud => NudHandler,
    .led => LedHandler,
    .bp => BindingPower,
} {
    if (switch (handler_type) {
        .statement => self.statement_lookup,
        .nud => self.nud_lookup,
        .led => self.led_lookup,
        .bp => self.bp_lookup,
    }.get(token)) |handler| {
        return handler;
    } else return utils.printErr(
        error.HandlerDoesNotExist,
        "Parser: {s} handler for '{}' does not exist.\n",
        .{ @tagName(handler_type), token },
        .red,
    );
}

/// parses parameters and returns `!Node.ParameterList`. Caller is responsible for cleanup.
pub fn parseParameters(self: *Self) !ast.ParameterList {
    return try self.parseParametersGeneric(false);
}

pub fn parseGenericParameters(self: *Self) ParserError!ast.ParameterList {
    return try self.parseParametersGeneric(true);
}

pub fn parseArguments(self: *Self) ParserError!ast.ArgumentList {
    var args: ast.ArgumentList = .empty;

    try self.expect(self.advance(), .open_paren, "argument list", "(");

    if (self.currentTokenKind() == Lexer.Token.close_paren) {
        _ = self.advance();
    } else while (true) {
        try args.append(self.alloc, try expression_handlers.parseExpression(self, .default));

        self.expectSilent(self.currentToken(), .comma) catch {
            try self.expect(self.advance(), .close_paren, "argument list", ")");
            break;
        };

        _ = self.advance();
    }

    return args;
}

pub fn parseBlock(self: *Self) !ast.Block {
    var block: ast.Block = .empty;

    try self.expect(self.advance(), .open_brace, "block", "{");

    while (self.currentTokenKind() != .eof and self.currentTokenKind() != .close_brace)
        try block.append(self.alloc, try statement_handlers.parseStatement(self));

    try self.expect(self.advance(), .close_brace, "block", "}");

    return block;
}

pub fn parseParametersGeneric(self: *Self, type_is_optional: bool) ParserError!ast.ParameterList {
    var params: ast.ParameterList = .empty;

    try self.expect(self.advance(), .open_paren, "parameter list", "(");

    if (self.currentTokenKind() == Lexer.Token.close_paren) {
        _ = self.advance();
    } else while (true) {
        const param_name = try self.expect(self.advance(), .ident, "parameter list", "parameter name");

        var param_type: ast.Type = .inferred;
        if (type_is_optional) {
            if (self.currentTokenKind() == Lexer.Token.colon) {
                _ = self.advance();
                param_type = try self.type_parser.parseType(self.alloc, .default);
            }
        } else {
            try self.expect(self.advance(), .colon, "parameter list", ":");
            param_type = try self.type_parser.parseType(self.alloc, .default);
        }

        try params.append(self.alloc, .{ .name = param_name, .type = param_type });

        // look for a comma, else a closing parenthesis
        self.expectSilent(self.currentToken(), .comma) catch {
            try self.expect(self.advance(), .close_paren, "parameter list", ")");
            break;
        };
        _ = self.advance();
    }

    return params;
}

/// Puts expression's hash code into `source_map` with `pos` as value and then returns it back.
pub inline fn putExprPos(self: *Self, expr: ast.Expression, pos: utils.Position) !ast.Expression {
    var h = std.hash.Wyhash.init(0);
    hash(&h, expr, 0);
    try self.source_map.put(h.final(), pos);
    return expr;
}

/// Gets expression from `source_map` by hash code.
/// `expr` must be an expression or a child of an expression. *const Expression will fail.
pub inline fn getExprPos(self: *const Self, expr: anytype) !utils.Position {
    var h = std.hash.Wyhash.init(0);
    hash(&h, expr, 0);
    return self.source_map.get(h.final()) orelse @panic("Expression not in map!\n");
}

/// Puts expression's hash code into `source_map` with `pos` as value and then returns it back.
pub inline fn putStatementPos(self: *Self, stmt: ast.Statement, pos: utils.Position) !ast.Statement {
    var h = std.hash.Wyhash.init(0);
    hash(&h, stmt, 0);
    try self.source_map.put(h.final(), pos);
    return stmt;
}

/// Gets expression from `source_map` by hash code.
pub inline fn getStatementPos(self: *const Self, stmt: anytype) !utils.Position {
    var h = std.hash.Wyhash.init(0);
    hash(&h, stmt, 0);
    return self.source_map.get(h.final()) orelse error.StatementNotInMap;
}

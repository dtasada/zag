const std = @import("std");
const pretty = @import("pretty");
const utils = @import("../utils.zig");
const ast = @import("ast.zig");

const statement_handlers = @import("statement_handlers.zig");
const expression_handlers = @import("expression_handlers.zig");

const Lexer = @import("../Lexer.zig");
const TypeParser = @import("TypeParser.zig");

const Self = @This();

const StatementHandler = *const fn (*Self, std.mem.Allocator) ParserError!ast.Statement;
const NudHandler = *const fn (*Self, std.mem.Allocator) ParserError!ast.Expression;
const LedHandler = *const fn (*Self, std.mem.Allocator, *const ast.Expression, BindingPower) ParserError!ast.Expression;

const StatementLookup = std.AutoHashMap(Lexer.TokenKind, StatementHandler);
const NudLookup = std.AutoHashMap(Lexer.TokenKind, NudHandler);
const LedLookup = std.AutoHashMap(Lexer.TokenKind, LedHandler);
const BpLookup = std.AutoHashMap(Lexer.TokenKind, BindingPower);

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
};

pos: usize,
input: *const Lexer,
bp_lookup: BpLookup,
nud_lookup: NudLookup,
led_lookup: LedLookup,
statement_lookup: StatementLookup,
type_parser: TypeParser,
// errors: std.ArrayList(ParserError) = .{},

pub fn init(input: *const Lexer, alloc: std.mem.Allocator) !*Self {
    const self = try alloc.create(Self);
    self.* = .{
        .pos = 0,
        .input = input,
        .bp_lookup = .init(alloc),
        .nud_lookup = .init(alloc),
        .led_lookup = .init(alloc),
        .statement_lookup = .init(alloc),
        .type_parser = try .init(alloc, self),
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
    try self.led(Lexer.Token.dot_dot, .logical, expression_handlers.parseBinaryExpression);

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
    try self.led(Lexer.Token.dot_dot, .call, expression_handlers.parseRangeExpression);

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

    return self;
}

pub fn deinit(self: *Self) void {
    self.bp_lookup.deinit();
    self.nud_lookup.deinit();
    self.led_lookup.deinit();
    self.statement_lookup.deinit();
}

/// Returns root node of the AST
pub fn getAst(self: *Self, alloc: std.mem.Allocator) !ast.RootNode {
    var root = ast.RootNode{};

    while (std.meta.activeTag(self.currentToken()) != Lexer.Token.eof)
        try root.append(alloc, try statement_handlers.parseStatement(self, alloc));

    return root;
}

/// Consumes current token and then increases position.
pub inline fn advance(self: *Self) Lexer.Token {
    const current_token = self.input.tokens.items[self.pos];
    self.pos += 1;
    return current_token;
}

pub inline fn currentToken(self: *const Self) Lexer.Token {
    return self.input.tokens.items[self.pos];
}

pub inline fn currentTokenKind(self: *const Self) Lexer.TokenKind {
    return std.meta.activeTag(self.currentToken());
}

/// Skips a token (self.pos+=2) and returns token at new position.
inline fn skipToken(self: *Self) Lexer.Token {
    self.pos += 2;
    return self.currentToken();
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
    const pos = std.math.clamp(self.pos, 0, self.input.source_map.items.len - 1);

    utils.print(
        "Unexpected token '{f}' in {s} at {}:{}. Expected '{s}'\n",
        .{
            actual,
            environment,
            self.input.source_map.items[pos].line,
            self.input.source_map.items[pos].col,
            expected_token,
        },
        .red,
    );
    return error.UnexpectedToken;
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
    } else {
        utils.print("Parser: {s} handler for '{}' does not exist.\n", .{ @tagName(handler_type), token }, .red);

        return error.HandlerDoesNotExist;
    }
}

/// parses parameters and returns `!Node.ParameterList`. Caller is responsible for cleanup.
pub fn parseParameters(self: *Self, alloc: std.mem.Allocator) !ast.ParameterList {
    return try self.parseParametersGeneric(alloc, false);
}

pub fn parseGenericParameters(self: *Self, alloc: std.mem.Allocator) ParserError!ast.ParameterList {
    return try self.parseParametersGeneric(alloc, true);
}

pub fn parseArguments(self: *Self, alloc: std.mem.Allocator) ParserError!ast.ArgumentList {
    var args = ast.ArgumentList{};

    try self.expect(self.advance(), .open_paren, "argument list", "(");

    if (self.currentTokenKind() == Lexer.Token.close_paren) {
        _ = self.advance();
    } else while (true) {
        try args.append(alloc, try expression_handlers.parseExpression(self, alloc, .default));

        self.expectSilent(self.currentToken(), .comma) catch {
            try self.expect(self.advance(), .close_paren, "argument list", ")");
            break;
        };

        _ = self.advance();
    }

    return args;
}

pub fn parseBlock(self: *Self, alloc: std.mem.Allocator) !ast.Block {
    var block = ast.Block{};

    try self.expect(self.advance(), .open_brace, "block", "{");

    while (self.currentTokenKind() != .eof and self.currentTokenKind() != .close_brace)
        try block.append(alloc, try statement_handlers.parseStatement(self, alloc));

    try self.expect(self.advance(), .close_brace, "block", "}");

    return block;
}

pub fn parseParametersGeneric(self: *Self, alloc: std.mem.Allocator, type_is_optional: bool) ParserError!ast.ParameterList {
    var params = ast.ParameterList{};

    try self.expect(self.advance(), .open_paren, "parameter list", "(");

    if (self.currentTokenKind() == Lexer.Token.close_paren) {
        _ = self.advance();
    } else while (true) {
        const param_name = try self.expect(self.advance(), .ident, "parameter list", "parameter name");

        var param_type: ast.Type = .inferred;
        if (type_is_optional) {
            if (self.currentTokenKind() == Lexer.Token.colon) {
                _ = self.advance();
                param_type = try self.type_parser.parseType(alloc, .default);
            }
        } else {
            try self.expect(self.advance(), .colon, "parameter list", ":");
            param_type = try self.type_parser.parseType(alloc, .default);
        }

        try params.append(alloc, .{ .name = param_name, .type = param_type });

        // look for a comma, else a closing parenthesis
        self.expectSilent(self.currentToken(), .comma) catch {
            try self.expect(self.advance(), .close_paren, "parameter list", ")");
            break;
        };
        _ = self.advance();
    }

    return params;
}

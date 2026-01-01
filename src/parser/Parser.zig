const std = @import("std");
const pretty = @import("pretty");
const utils = @import("utils");
pub const ast = @import("ast.zig");

const statements = @import("statements.zig");
const expressions = @import("expressions.zig");

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
        .alloc = alloc,
    };

    try self.led(.equals, .assignment, expressions.assignment);
    try self.led(.plus_equals, .assignment, expressions.assignment);
    try self.led(.minus_equals, .assignment, expressions.assignment);
    try self.led(.times_equals, .assignment, expressions.assignment);
    try self.led(.slash_equals, .assignment, expressions.assignment);
    try self.led(.mod_equals, .assignment, expressions.assignment);
    try self.led(.and_equals, .assignment, expressions.assignment);
    try self.led(.or_equals, .assignment, expressions.assignment);
    try self.led(.xor_equals, .assignment, expressions.assignment);
    try self.led(.shift_right_equals, .assignment, expressions.assignment);
    try self.led(.shift_left_equals, .assignment, expressions.assignment);

    // logical
    try self.led(.@"and", .logical, expressions.binary);
    try self.led(.@"or", .logical, expressions.binary);

    // relational
    try self.led(.less, .relational, expressions.binary);
    try self.led(.less_equals, .relational, expressions.binary);
    try self.led(.greater, .relational, expressions.binary);
    try self.led(.greater_equals, .relational, expressions.binary);
    try self.led(.equals_equals, .relational, expressions.binary);
    try self.led(.bang_equals, .relational, expressions.binary);

    // additive & multiplicative
    try self.led(.plus, .additive, expressions.binary);
    try self.led(.dash, .additive, expressions.binary);
    try self.led(.asterisk, .multiplicative, expressions.binary);
    try self.led(.slash, .multiplicative, expressions.binary);
    try self.led(.percent, .multiplicative, expressions.binary);
    try self.led(.ampersand, .multiplicative, expressions.binary);
    try self.led(.pipe, .additive, expressions.binary);
    try self.led(.caret, .additive, expressions.binary);
    try self.led(.pipe, .additive, expressions.binary);
    try self.led(.caret, .additive, expressions.binary);
    try self.led(.shift_left, .multiplicative, expressions.binary);
    try self.led(.shift_right, .multiplicative, expressions.binary);

    // literals & symbols
    try self.nud(.int, expressions.primary);
    try self.nud(.float, expressions.primary);
    try self.nud(.ident, expressions.primary);
    try self.nud(.string, expressions.primary);
    try self.nud(.dash, expressions.prefix);
    try self.nud(.open_paren, expressions.group);

    // Call/member expressions
    try self.led(.dot, .member, expressions.member);
    try self.led(.open_brace, .call, expressions.structInstantiation);
    try self.led(.open_paren, .call, expressions.call);
    try self.nud(.open_bracket, expressions.arrayInstantiation);

    // other expressions
    try self.nud(.open_brace, expressions.block);
    try self.nud(.@"if", expressions.@"if");
    try self.nud(.ampersand, expressions.reference);
    try self.led(.dot_dot, .relational, expressions.range);
    try self.led(.dot_dot_equals, .relational, expressions.range);
    try self.led(.open_bracket, .call, expressions.index);

    // Statements
    try self.statement(.let, statements.variableDeclaration);
    try self.statement(.mut, statements.variableDeclaration);
    try self.statement(.@"struct", statements.structDeclaration);
    try self.statement(.@"enum", statements.enumDeclaration);
    try self.statement(.@"union", statements.unionDeclaration);
    try self.statement(.@"fn", statements.functionDefinition);
    try self.statement(.@"while", statements.@"while");
    try self.statement(.@"return", statements.@"return");
    try self.statement(.@"for", statements.@"for");
    try self.statement(.@"if", statements.@"if");

    while (std.meta.activeTag(self.currentToken()) != Lexer.Token.eof)
        try self.output.append(self.alloc, try statements.parse(self));

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
        try args.append(self.alloc, try expressions.parse(self, .default));

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
        try block.append(self.alloc, try statements.parse(self));

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

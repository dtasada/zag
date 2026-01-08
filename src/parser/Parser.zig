const std = @import("std");
const pretty = @import("pretty");
const utils = @import("utils");
pub const ast = @import("ast.zig");

const statements = @import("statements.zig");
const expressions = @import("expressions.zig");

const Lexer = @import("Lexer");
const TypeParser = @import("TypeParser.zig");

const Self = @This();

/// Function signature for a statement handler.
const StatementHandler = *const fn (*Self) ParserError!ast.Statement;

/// Function signature for a nud handler.
const NudHandler = *const fn (*Self) ParserError!ast.Expression;

/// Function signature for a led handler.
const LedHandler = *const fn (*Self, *const ast.Expression, BindingPower) ParserError!ast.Expression;

/// Binding power. Order of this enum is functional, so don't change it.
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
    MissingReturnType,
    SyntaxError,
};

/// Parser state: the current reading position in the lexer token list.
pos: usize,

/// Input tokens, and maps the tokens to the source code.
lexer: *const Lexer,

alloc: std.mem.Allocator,

/// Parser output: the root AST object.
output: ast.RootNode = .empty,

/// pratt parsing helpers
type_parser: TypeParser,

/// Maps a `Token` to its corresponding binding power.
bp_lookup: std.AutoHashMap(Lexer.TokenKind, BindingPower),

/// Maps a `Token` to a corresponding nud handler.
nud_lookup: std.AutoHashMap(Lexer.TokenKind, NudHandler),

/// Maps a `Token` to a corresponding led handler.
led_lookup: std.AutoHashMap(Lexer.TokenKind, LedHandler),

/// Maps a `Token` to a corresponding statement handler.
statement_lookup: std.AutoHashMap(Lexer.TokenKind, StatementHandler),

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

    try self.led(.@"=", .assignment, expressions.assignment);
    try self.led(.@"+=", .assignment, expressions.assignment);
    try self.led(.@"-=", .assignment, expressions.assignment);
    try self.led(.@"*=", .assignment, expressions.assignment);
    try self.led(.@"/=", .assignment, expressions.assignment);
    try self.led(.@"%=", .assignment, expressions.assignment);
    try self.led(.@"&=", .assignment, expressions.assignment);
    try self.led(.@"|=", .assignment, expressions.assignment);
    try self.led(.@"^=", .assignment, expressions.assignment);
    try self.led(.@">>=", .assignment, expressions.assignment);
    try self.led(.@"<<=", .assignment, expressions.assignment);

    // logical
    try self.led(.@"and", .logical, expressions.binary);
    try self.led(.@"or", .logical, expressions.binary);

    // relational
    try self.led(.@"<", .relational, expressions.binary);
    try self.led(.@"<=", .relational, expressions.binary);
    try self.led(.@">", .relational, expressions.binary);
    try self.led(.@">=", .relational, expressions.binary);
    try self.led(.@"==", .relational, expressions.binary);
    try self.led(.@"!=", .relational, expressions.binary);

    // additive & multiplicative
    try self.led(.@"+", .additive, expressions.binary);
    try self.led(.@"-", .additive, expressions.binary);
    try self.led(.@"*", .multiplicative, expressions.binary);
    try self.led(.@"/", .multiplicative, expressions.binary);
    try self.led(.@"*", .multiplicative, expressions.binary);
    try self.led(.@"&", .multiplicative, expressions.binary);
    try self.led(.@"|", .additive, expressions.binary);
    try self.led(.@"^", .additive, expressions.binary);
    try self.led(.@"<<", .multiplicative, expressions.binary);
    try self.led(.@">>", .multiplicative, expressions.binary);

    // literals & symbols
    try self.nud(.int, expressions.primary);
    try self.nud(.float, expressions.primary);
    try self.nud(.ident, expressions.primary);
    try self.nud(.string, expressions.primary);
    try self.nud(.@"-", expressions.prefix);
    try self.nud(.open_paren, expressions.group);

    // Call/member expressions
    try self.led(.@".", .member, expressions.member);
    try self.led(.open_brace, .call, expressions.structInstantiation);
    try self.led(.open_paren, .call, expressions.call);
    try self.nud(.open_bracket, expressions.arrayInstantiation);

    // other expressions
    try self.nud(.open_brace, expressions.block);
    try self.nud(.@"if", expressions.@"if");
    try self.nud(.@"&", expressions.reference);
    try self.led(.dot_dot, .relational, expressions.range);
    try self.led(.dot_dot_equals, .relational, expressions.range);
    try self.led(.open_bracket, .call, expressions.index);

    // Statements
    try self.statement(.let, statements.variableDeclaration);
    try self.statement(.@"struct", statements.structDeclaration);
    try self.statement(.@"enum", statements.enumDeclaration);
    try self.statement(.@"union", statements.unionDeclaration);
    try self.statement(.@"fn", statements.functionDefinition);
    try self.statement(.bind, statements.bindingFunctionDeclaration);
    try self.statement(.@"while", statements.@"while");
    try self.statement(.@"return", statements.@"return");
    try self.statement(.@"for", statements.@"for");
    try self.statement(.@"if", statements.@"if");
    try self.statement(.import, statements.import);
    try self.statement(.@"pub", statements.@"pub");

    while (std.meta.activeTag(self.currentToken()) != Lexer.Token.eof)
        try self.output.append(self.alloc, try statements.parse(self));

    return self;
}

/// Cleans up resources
pub fn deinit(self: *Self) void {
    self.bp_lookup.deinit();
    self.nud_lookup.deinit();
    self.led_lookup.deinit();
    self.statement_lookup.deinit();
}

/// Returns the line and column in the source file corresponding to what is being parsed.
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

/// Returns token at the current position.
pub inline fn currentToken(self: *const Self) Lexer.Token {
    return self.lexer.tokens.items[self.pos];
}

/// Returns token at the current position.
pub inline fn previousToken(self: *const Self) Lexer.Token {
    return self.lexer.tokens.items[self.pos - 1];
}

/// Returns the tag of the token at the current position.
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

/// Statements are standalone objects that begin with a token and don't rely on any other state.
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

/// Parses parameters and returns `!Node.ParameterList`. Caller is responsible for cleanup.
pub fn parseParameters(self: *Self) !ast.ParameterList {
    return try self.parseParametersGeneric(false);
}

/// Parses generic parameter list.
/// Equivalent to a normal parameter list but the explicit type is optional.
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
    } else {
        var last_arg = false;
        while (!last_arg) {
            const param_name = try self.expect(self.advance(), .ident, "parameter list", "parameter name");

            var param_type: ast.Type = .inferred;
            if (type_is_optional and self.currentTokenKind() == Lexer.Token.colon) {
                _ = self.advance();
                param_type = try self.type_parser.parseType(self.alloc, .default);
            } else switch (self.advance()) {
                .colon => param_type = try self.type_parser.parseType(self.alloc, .default),
                .dot_dot_dot => {
                    param_type = .variadic;
                    last_arg = true;
                },
                else => |actual| return utils.printErr(
                    error.UnexpectedToken,
                    "Unexpected token '{f}' in parameter list at {f}. Expected an explicit or variadic type.\n",
                    .{ actual, self.lexer.source_map.items[
                        std.math.clamp(
                            self.pos - 1,
                            0,
                            self.lexer.source_map.items.len - 1,
                        )
                    ] },
                    .red,
                ),
            }

            try params.append(self.alloc, .{ .name = param_name, .type = param_type });

            // look for a comma, else a closing parenthesis
            self.expectSilent(self.currentToken(), .comma) catch {
                try self.expect(self.advance(), .close_paren, "parameter list", ")");
                break;
            };

            _ = self.advance();
        }
    }

    return params;
}

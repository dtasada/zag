const std = @import("std");
const pretty = @import("pretty");
const utils = @import("utils.zig");

const Lexer = @import("Lexer.zig");
const TypeParser = @import("TypeParser.zig");

const ast = @import("ast.zig");

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

    try self.led(Lexer.Token.equals, .assignment, parseAssignmentExpression);
    try self.led(Lexer.Token.plus_equals, .assignment, parseAssignmentExpression);
    try self.led(Lexer.Token.minus_equals, .assignment, parseAssignmentExpression);
    try self.led(Lexer.Token.times_equals, .assignment, parseAssignmentExpression);
    try self.led(Lexer.Token.slash_equals, .assignment, parseAssignmentExpression);

    // logical
    try self.led(Lexer.Token.@"and", .logical, parseBinaryExpression);
    try self.led(Lexer.Token.@"or", .logical, parseBinaryExpression);
    try self.led(Lexer.Token.dot_dot, .logical, parseBinaryExpression);

    // relational
    try self.led(Lexer.Token.less, .relational, parseBinaryExpression);
    try self.led(Lexer.Token.less_equals, .relational, parseBinaryExpression);
    try self.led(Lexer.Token.greater, .relational, parseBinaryExpression);
    try self.led(Lexer.Token.greater_equals, .relational, parseBinaryExpression);
    try self.led(Lexer.Token.equals_equals, .relational, parseBinaryExpression);
    try self.led(Lexer.Token.bang_equals, .relational, parseBinaryExpression);

    // additive & multiplicative
    try self.led(Lexer.Token.plus, .additive, parseBinaryExpression);
    try self.led(Lexer.Token.dash, .additive, parseBinaryExpression);
    try self.led(Lexer.Token.asterisk, .multiplicative, parseBinaryExpression);
    try self.led(Lexer.Token.slash, .multiplicative, parseBinaryExpression);
    try self.led(Lexer.Token.percent, .multiplicative, parseBinaryExpression);

    // literals & symbols
    try self.nud(Lexer.Token.int, parsePrimaryExpression);
    try self.nud(Lexer.Token.float, parsePrimaryExpression);
    try self.nud(Lexer.Token.ident, parsePrimaryExpression);
    try self.nud(Lexer.Token.string, parsePrimaryExpression);
    try self.nud(Lexer.Token.dash, parsePrefixExpression);
    try self.nud(Lexer.Token.open_paren, parseGroupExpression);

    // Call/member expressions
    try self.led(Lexer.Token.open_brace, .call, parseStructInstantiationExpression);
    try self.led(Lexer.Token.open_paren, .call, parseCallExpression);
    try self.nud(Lexer.Token.open_bracket, parseArrayInstantiationExpression);
    try self.nud(Lexer.Token.@"if", parseIfExpression);

    // other expressions

    // Statements
    try self.statement(Lexer.Token.let, parseVariableDeclarationStatement);
    try self.statement(Lexer.Token.@"var", parseVariableDeclarationStatement);
    try self.statement(Lexer.Token.@"struct", parseStructDeclarationStatement);
    try self.statement(Lexer.Token.@"fn", parseFunctionDefinition);
    try self.statement(Lexer.Token.@"if", parseIfStatement);

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
        try root.append(alloc, try self.parseStatement(alloc));

    return root;
}

/// Consumes current token and then increases position.
pub inline fn advance(self: *Self) Lexer.Token {
    const current_token = self.input.tokens.items[self.pos];
    self.pos += 1;
    return current_token;
}

/// Doesn't change position and returns next token.
inline fn peek(self: *const Self) Lexer.Token {
    return self.input.tokens.items[self.pos + 1];
}

pub inline fn currentToken(self: *const Self) Lexer.Token {
    return self.input.tokens.items[self.pos];
}

pub inline fn currentTokenKind(self: *const Self) Lexer.TokenKind {
    return std.meta.activeTag(self.currentToken());
}

/// Consumes token and returns next one.
inline fn nextToken(self: *Self) Lexer.Token {
    self.pos += 1;
    return self.currentToken();
}

/// Skips a token (self.pos+=2) and returns token at new position.
inline fn skipToken(self: *Self) Lexer.Token {
    self.pos += 2;
    return self.currentToken();
}

/// parses parameters and returns `!Node.ParameterList`. Caller is responsible for cleanup.
fn parseParameters(self: *Self, alloc: std.mem.Allocator) !ast.ParameterList {
    var params = ast.ParameterList{};

    try self.expect(self.advance(), .open_paren, "parameter list", "'('");

    while (true) {
        const param_name = try self.expect(self.advance(), .ident, "parameter list", "parameter name");
        try self.expect(self.advance(), .colon, "parameter list", ":");
        const param_type = try self.type_parser.parseType(alloc, .default);

        try params.append(alloc, .{ .param_name = param_name, .type = param_type });

        // look for a comma, else a closing parenthesis
        self.expectSilent(self.currentToken(), .comma) catch {
            try self.expect(self.advance(), .close_paren, "parameter list", ")");
            break;
        };
        _ = self.advance();
    }

    return params;
}

fn parseArguments(self: *Self, alloc: std.mem.Allocator) ParserError!ast.ArgumentList {
    var args = ast.ArgumentList{};

    try self.expect(self.advance(), .open_paren, "argument list", "(");

    while (true) {
        try args.append(alloc, try self.parseExpression(alloc, .default));

        self.expectSilent(self.currentToken(), .comma) catch {
            try self.expect(self.advance(), .close_paren, "parameter list", ")");
            break;
        };

        _ = self.advance();
    }

    return args;
}

fn parseBlock(self: *Self, alloc: std.mem.Allocator) !ast.Block {
    var block = ast.Block{};

    try self.expect(self.advance(), .open_brace, "block", "{");

    while (self.currentTokenKind() != .eof and self.currentTokenKind() != .close_brace) {
        try block.append(alloc, try self.parseStatement(alloc));
    }

    try self.expect(self.advance(), .close_brace, "block", "'}'");

    return block;
}

fn parseStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    if (self.statement_lookup.get(self.currentTokenKind())) |statement_fn| {
        return statement_fn(self, alloc);
    }

    const expression = try self.parseExpression(alloc, .default);
    try self.expect(self.currentToken(), Lexer.Token.semicolon, "statement", ";");
    _ = self.advance(); // consume semicolon

    return .{ .expression = expression };
}

fn parsePrimaryExpression(self: *Self, _: std.mem.Allocator) !ast.Expression {
    return switch (self.advance()) {
        .int => |int| .{ .uint = int },
        .float => |float| .{ .float = float },
        .ident => |atom| .{ .ident = atom },
        .string => |string| .{ .string = string },
        else => |other| self.unexpectedToken("primary expression", "int, float, atom", other),
    };
}

fn parseBinaryExpression(self: *Self, alloc: std.mem.Allocator, lhs: *const ast.Expression, bp: BindingPower) ParserError!ast.Expression {
    const op = ast.BinaryOperator.fromLexerToken(self.advance());
    const rhs = try self.parseExpression(alloc, bp);

    const new_rhs = try alloc.create(ast.Expression);
    new_rhs.* = rhs;

    return .{
        .binary = .{
            .lhs = lhs,
            .op = op,
            .rhs = new_rhs,
        },
    };
}

fn parseExpression(self: *Self, alloc: std.mem.Allocator, bp: BindingPower) ParserError!ast.Expression {
    // first parse the NUD
    const token_kind = self.currentTokenKind();
    const nud_fn = try self.getHandler(.nud, token_kind);

    const lhs = try alloc.create(ast.Expression);
    lhs.* = try nud_fn(self, alloc);

    // while we have a led and (current bp < bp of current token)
    // continue parsing lhs

    while (self.bp_lookup.get(self.currentTokenKind())) |current_bp| {
        if (@intFromEnum(current_bp) <= @intFromEnum(bp)) break;

        // This should be an assertion, since we found a bp
        const led_fn = try self.getHandler(.led, self.currentTokenKind());
        lhs.* = try led_fn(self, alloc, lhs, try self.getHandler(.bp, self.currentTokenKind()));
    }

    return lhs.*;
}

fn parseVariableDeclarationStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    const is_mut = std.meta.activeTag(self.advance()) == Lexer.Token.@"var";
    const var_name = try self.expect(
        self.advance(),
        Lexer.Token.ident,
        "variable declaration statement",
        "variable name",
    );

    // optionally parse type
    var @"type": ast.Type = .inferred;
    if (self.currentTokenKind() == Lexer.Token.colon) {
        _ = self.advance(); // consume colon
        @"type" = try self.type_parser.parseType(alloc, .default);
    }

    try self.expect(
        self.advance(),
        Lexer.Token.equals,
        "variable declaration statement",
        "=",
    );

    const assigned_value = try self.parseExpression(alloc, .assignment);

    try self.expect(
        self.currentToken(),
        Lexer.Token.semicolon,
        "variable declaration statement",
        ";",
    );
    _ = self.advance(); // consume semicolon

    return .{
        .variable_declaration = .{
            .variable_name = var_name,
            .is_mut = is_mut,
            .assigned_value = assigned_value,
            .type = @"type",
        },
    };
}

fn parseAssignmentExpression(self: *Self, alloc: std.mem.Allocator, lhs: *const ast.Expression, bp: BindingPower) ParserError!ast.Expression {
    const op = self.advance();

    const rhs = try alloc.create(ast.Expression);
    rhs.* = try self.parseExpression(alloc, bp);

    return .{
        .assignment = .{
            .assignee = lhs,
            .op = op,
            .value = rhs,
        },
    };
}

fn parsePrefixExpression(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Expression {
    const op = self.advance();

    const rhs = try alloc.create(ast.Expression);
    rhs.* = try self.parseExpression(alloc, .default);

    return .{
        .prefix = .{
            .op = op,
            .rhs = rhs,
        },
    };
}

fn parseGroupExpression(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Expression {
    _ = self.advance();
    const expr = try self.parseExpression(alloc, .default);
    try self.expect(self.currentToken(), Lexer.Token.close_paren, "group expression", ")");
    _ = self.advance();

    return expr;
}

fn parseStructDeclarationStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    try self.expect(self.advance(), Lexer.Token.@"struct", "struct declaration", "struct");

    var @"struct" = ast.Statement{
        .struct_declaration = .{
            .name = try self.expect(self.advance(), Lexer.Token.ident, "struct declaration", "struct name"),
        },
    };

    try self.expect(self.advance(), Lexer.Token.open_brace, "struct declaration", "{");

    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        // parse member
        switch (self.currentToken()) {
            .ident => {
                const member_name = try self.expect(self.advance(), Lexer.Token.ident, "struct declaration", "member name");
                try self.expect(self.advance(), Lexer.Token.colon, "struct declaration", ":");
                const member_type = try self.type_parser.parseType(alloc, .default);
                try self.expect(self.advance(), Lexer.Token.semicolon, "struct declaration", ";");

                try @"struct".struct_declaration.members.append(alloc, .{
                    .name = member_name,
                    .type = member_type,
                });
            },
            .@"fn" => try @"struct".struct_declaration.methods.append(
                alloc,
                (try self.parseFunctionDefinition(alloc)).function_definition,
            ),
            else => unreachable,
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, "struct declaration", "}");

    return @"struct";
}

fn parseStructInstantiationExpression(self: *Self, alloc: std.mem.Allocator, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    const struct_name = switch (lhs.*) {
        .ident => |ident| ident,
        else => |other| {
            utils.print("Parser: Expected struct name in struct instantiation, received {}.", .{other}, .red);
            return error.UnexpectedExpression;
        },
    };

    try self.expect(self.advance(), Lexer.Token.open_brace, "struct instantiation", "{");

    var @"struct" = ast.Expression.StructInstantiation{
        .name = struct_name,
        .members = .init(alloc),
    };

    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        const member_name = try self.expect(self.advance(), Lexer.Token.ident, "struct instantiation", "struct member name");
        try self.expect(self.advance(), Lexer.Token.colon, "struct instantiation", ":");
        const member_value = try self.parseExpression(alloc, .default);

        try @"struct".members.put(member_name, member_value);

        if (self.currentTokenKind() != Lexer.Token.close_brace) {
            try self.expect(self.advance(), Lexer.Token.comma, "struct instantiation", ",");
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, "struct instantiation", "}");

    return .{ .struct_instantiation = @"struct" };
}

fn parseArrayInstantiationExpression(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Expression {
    var array = ast.Expression.ArrayInstantiation{ .type = undefined };

    try self.expect(self.advance(), Lexer.Token.open_bracket, "array instantiation", "[");
    try self.expect(self.advance(), Lexer.Token.close_bracket, "array instantiation", "]");

    array.type = try self.type_parser.parseType(alloc, .default);

    try self.expect(self.advance(), Lexer.Token.open_brace, "array instantiation", "{");
    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        try array.contents.append(alloc, try self.parseExpression(alloc, .logical));

        if (self.currentTokenKind() != Lexer.Token.close_brace)
            try self.expect(self.advance(), Lexer.Token.comma, "array literal", ",");
    }
    try self.expect(self.advance(), Lexer.Token.close_brace, "array instantiation", "}");

    return .{ .array_instantiation = array };
}

fn parseCallExpression(self: *Self, alloc: std.mem.Allocator, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    return .{
        .call = .{
            .callee = lhs,
            .args = try self.parseArguments(alloc),
        },
    };
}

fn parseFunctionDefinition(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    try self.expect(self.advance(), Lexer.Token.@"fn", "function definition", "fn");
    const function_name = try self.expect(self.advance(), Lexer.Token.ident, "function definition", "function name");
    const parameters = try self.parseParameters(alloc);
    const return_type = try self.type_parser.parseType(alloc, .default);
    const body = try self.parseBlock(alloc);

    return .{
        .function_definition = .{
            .name = function_name,
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
        },
    };
}

fn parseIfExpression(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Expression {
    try self.expect(self.advance(), Lexer.Token.@"if", "if statement", "if");

    try self.expect(self.advance(), Lexer.Token.open_paren, "array instantiation", "(");

    const condition = try alloc.create(ast.Expression);
    condition.* = try self.parseExpression(alloc, .default);

    try self.expect(self.advance(), Lexer.Token.close_paren, "array instantiation", ")");

    var body = ast.Block{};
    if (self.parseStatement(alloc) catch null) |body_statement|
        try body.append(alloc, body_statement)
    else
        body = try self.parseBlock(alloc);

    return .{
        .@"if" = .{
            .condition = condition,
            .body = body,
        },
    };
}

fn parseIfStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    return .{ .expression = try self.parseIfExpression(alloc) };
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
fn unexpectedToken(
    self: *const Self,
    environment: []const u8,
    expected_token: []const u8,
    actual: Lexer.Token,
) error{ NoSpaceLeft, UnexpectedToken } {
    utils.print(
        "Unexpected token '{f}' in {s} at {}:{}. Expected '{s}'\n",
        .{
            actual,
            environment,
            self.input.source_map.items[self.pos].line,
            self.input.source_map.items[self.pos].col,
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
    comptime environment: []const u8,
    comptime expected_token: []const u8,
) !@FieldType(Lexer.Token, @tagName(expected)) {
    return if (std.meta.activeTag(actual) == expected)
        @field(actual, @tagName(expected))
    else
        self.unexpectedToken(environment, expected_token, actual);
}

/// Identical to `expect` but doesn't print error message.
fn expectSilent(
    _: *const Self,
    actual: Lexer.Token,
    comptime expected: Lexer.TokenKind,
) !@FieldType(Lexer.Token, @tagName(expected)) {
    return if (std.meta.activeTag(actual) == expected)
        @field(actual, @tagName(expected))
    else
        return error.UnexpectedToken;
}

inline fn getHandler(
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

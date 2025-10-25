const std = @import("std");
const pretty = @import("pretty");
const utils = @import("../utils.zig");
const ast = @import("ast.zig");
const statement_handlers = @import("statement_handlers.zig");

const Lexer = @import("../Lexer.zig");
const TypeParser = @import("TypeParser.zig");

const Self = @import("Parser.zig");
const ParserError = Self.ParserError;
const BindingPower = Self.BindingPower;

pub fn parsePrimaryExpression(self: *Self, _: std.mem.Allocator) !ast.Expression {
    return switch (self.advance()) {
        .int => |int| .{ .uint = int },
        .float => |float| .{ .float = float },
        .ident => |atom| .{ .ident = atom },
        .string => |string| .{ .string = string },
        else => |other| self.unexpectedToken("primary expression", "int, float, atom", other),
    };
}

pub fn parseBinaryExpression(self: *Self, alloc: std.mem.Allocator, lhs: *const ast.Expression, bp: BindingPower) ParserError!ast.Expression {
    const op = ast.BinaryOperator.fromLexerToken(self.advance());
    const rhs = try parseExpression(self, alloc, bp);

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

pub fn parseExpression(self: *Self, alloc: std.mem.Allocator, bp: BindingPower) ParserError!ast.Expression {
    // first parse the NUD
    std.debug.print("curren token: {f}\n", .{self.currentToken()});
    const nud_fn = try self.getHandler(.nud, self.currentTokenKind());
    var lhs = try nud_fn(self, alloc);

    // while we have a led and (current bp < bp of current token)
    // continue parsing lhs
    while (self.bp_lookup.get(self.currentTokenKind())) |current_bp| {
        if (@intFromEnum(current_bp) <= @intFromEnum(bp)) break;

        // This should be an assertion, since we found a bp
        const led_fn = try self.getHandler(.led, self.currentTokenKind());

        const old_lhs = try alloc.create(ast.Expression);
        old_lhs.* = lhs;

        lhs = try led_fn(self, alloc, old_lhs, try self.getHandler(.bp, self.currentTokenKind()));
    }

    return lhs;
}

pub fn parseAssignmentExpression(self: *Self, alloc: std.mem.Allocator, lhs: *const ast.Expression, bp: BindingPower) ParserError!ast.Expression {
    const op = self.advance();

    const rhs = try alloc.create(ast.Expression);
    rhs.* = try parseExpression(self, alloc, bp);

    return .{
        .assignment = .{
            .assignee = lhs,
            .op = op,
            .value = rhs,
        },
    };
}

pub fn parseMemberAccessExpression(self: *Self, alloc: std.mem.Allocator, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    _ = self.advance(); // consume dot
    const member_name = try self.expect(self.advance(), .ident, "member expression", "member name");

    const rhs = try alloc.create(ast.Expression);
    rhs.* = .{ .ident = member_name };

    return .{
        .member = .{
            .lhs = lhs,
            .rhs = rhs,
        },
    };
}

pub fn parsePrefixExpression(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Expression {
    const op = self.advance();

    const rhs = try alloc.create(ast.Expression);
    rhs.* = try parseExpression(self, alloc, .default);

    return .{
        .prefix = .{
            .op = op,
            .rhs = rhs,
        },
    };
}

pub fn parseGroupExpression(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Expression {
    try self.expect(self.currentToken(), Lexer.Token.open_paren, "group expression", "(");
    const expr = try parseExpression(self, alloc, .default);
    try self.expect(self.currentToken(), Lexer.Token.close_paren, "group expression", ")");
    _ = self.advance();

    return expr;
}

pub fn parseStructInstantiationExpression(self: *Self, alloc: std.mem.Allocator, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
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
        const member_value = try parseExpression(self, alloc, .default);

        try @"struct".members.put(member_name, member_value);

        if (self.currentTokenKind() != Lexer.Token.close_brace) {
            try self.expect(self.advance(), Lexer.Token.comma, "struct instantiation", ",");
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, "struct instantiation", "}");

    return .{ .struct_instantiation = @"struct" };
}

pub fn parseArrayInstantiationExpression(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Expression {
    var array = ast.Expression.ArrayInstantiation{ .type = undefined };

    try self.expect(self.advance(), Lexer.Token.open_bracket, "array instantiation", "[");
    try self.expect(self.advance(), Lexer.Token.close_bracket, "array instantiation", "]");

    array.type = try self.type_parser.parseType(alloc, .default);

    try self.expect(self.advance(), Lexer.Token.open_brace, "array instantiation", "{");
    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        try array.contents.append(alloc, try parseExpression(self, alloc, .logical));

        if (self.currentTokenKind() != Lexer.Token.close_brace)
            try self.expect(self.advance(), Lexer.Token.comma, "array literal", ",");
    }
    try self.expect(self.advance(), Lexer.Token.close_brace, "array instantiation", "}");

    return .{ .array_instantiation = array };
}

pub fn parseCallExpression(self: *Self, alloc: std.mem.Allocator, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    return .{
        .call = .{
            .callee = lhs,
            .args = try self.parseArguments(alloc),
        },
    };
}

pub fn parseIfExpression(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Expression {
    try self.expect(self.advance(), Lexer.Token.@"if", "if statement", "if");

    try self.expect(self.advance(), Lexer.Token.open_paren, "if statement", "(");

    const condition = try alloc.create(ast.Expression);
    condition.* = try parseExpression(self, alloc, .default);

    try self.expect(self.advance(), Lexer.Token.close_paren, "if statement", ")");

    const capture: ?[]const u8 = switch (self.currentToken()) {
        .pipe => blk: {
            _ = self.advance(); // consume opening pipe
            const capture_name = try self.expect(self.advance(), Lexer.Token.ident, "capture", "capture name");
            try self.expect(self.advance(), Lexer.Token.pipe, "capture", "|"); // consume closing pipe
            break :blk capture_name;
        },
        else => null,
    };

    const body = try alloc.create(ast.Expression);
    body.* = if (self.currentTokenKind() == .open_brace)
        try parseBlockExpression(self, alloc)
    else
        try parseExpression(self, alloc, .default);

    var @"else": ?*ast.Expression = null;
    if (self.currentTokenKind() == Lexer.Token.@"else") {
        _ = self.advance(); // consume `else`

        @"else" = try alloc.create(ast.Expression);
        @"else".?.* = if (self.currentTokenKind() == .open_brace)
            try parseBlockExpression(self, alloc)
        else
            try parseExpression(self, alloc, .default);
    }

    return .{
        .@"if" = .{
            .condition = condition,
            .capture = capture,
            .body = body,
            .@"else" = @"else",
        },
    };
}

pub fn parseBlockExpression(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Expression {
    return .{ .block = try self.parseBlock(alloc) };
}

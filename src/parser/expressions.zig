const std = @import("std");
const pretty = @import("pretty");
const utils = @import("utils");
const ast = @import("ast.zig");
const statements = @import("statements.zig");

const Lexer = @import("Lexer");
const TypeParser = @import("TypeParser.zig");

const Self = @import("Parser.zig");
const ParserError = Self.ParserError;
const BindingPower = Self.BindingPower;

pub fn primary(self: *Self) !ast.Expression {
    const pos = self.currentPosition();

    const kind: ast.Expression = switch (self.advance()) {
        .int => |int| .{ .uint = int },
        .float => |float| .{ .float = float },
        .ident => |atom| .{ .ident = atom },
        .string => |string| .{ .string = string },
        .char => |char| .{ .char = char },
        else => |other| return self.unexpectedToken("primary expression", "(int | float | ident | string | char)", other),
    };

    return self.putExprPos(kind, pos);
}

pub fn binary(self: *Self, lhs: *const ast.Expression, bp: BindingPower) ParserError!ast.Expression {
    const pos = self.currentPosition();
    const op: ast.BinaryOperator = .fromLexerToken(self.advance());
    const rhs = try parse(self, bp);

    const new_rhs = try self.alloc.create(ast.Expression);
    new_rhs.* = rhs;

    return self.putExprPos(.{
        .binary = .{
            .lhs = lhs,
            .op = op,
            .rhs = new_rhs,
        },
    }, pos);
}

pub fn parse(self: *Self, bp: BindingPower) ParserError!ast.Expression {
    const pos = self.currentPosition();

    // first parse the NUD
    const nud_fn = try self.getHandler(.nud, self.currentTokenKind());
    var lhs = try nud_fn(self);

    // while we have a led and (current bp < bp of current token)
    // continue parsing lhs
    while (self.bp_lookup.get(self.currentTokenKind())) |current_bp| {
        if (@intFromEnum(current_bp) <= @intFromEnum(bp)) break;

        // This should be an assertion, since we found a bp
        const led_fn = try self.getHandler(.led, self.currentTokenKind());

        const old_lhs = try self.alloc.create(ast.Expression);
        old_lhs.* = lhs;

        lhs = try led_fn(self, old_lhs, try self.getHandler(.bp, self.currentTokenKind()));
    }

    return self.putExprPos(lhs, pos);
}

pub fn assignment(self: *Self, lhs: *const ast.Expression, bp: BindingPower) ParserError!ast.Expression {
    const pos = self.currentPosition();
    const op = self.advance();

    const rhs = try self.alloc.create(ast.Expression);
    rhs.* = try parse(self, bp);

    return self.putExprPos(.{
        .assignment = .{
            .assignee = lhs,
            .op = .fromLexerToken(op),
            .value = rhs,
        },
    }, pos);
}

pub fn member(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    const pos = self.currentPosition();
    _ = self.advance(); // consume dot
    const member_name = try self.expect(self.advance(), .ident, "member expression", "member name");

    return self.putExprPos(.{
        .member = .{
            .parent = lhs,
            .member_name = member_name,
        },
    }, pos);
}

pub fn prefix(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();
    const op = self.advance();

    const rhs = try self.alloc.create(ast.Expression);
    rhs.* = try parse(self, .unary);

    return self.putExprPos(.{
        .prefix = .{
            .op = .fromLexerToken(op),
            .rhs = rhs,
        },
    }, pos);
}

pub fn group(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();

    try self.expect(self.advance(), Lexer.Token.open_paren, "group expression", "(");
    const expr = try parse(self, .default);
    try self.expect(self.advance(), Lexer.Token.close_paren, "group expression", ")");

    return self.putExprPos(expr, pos);
}

pub fn structInstantiation(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    const struct_name = switch (lhs.*) {
        .ident => |ident| ident,
        else => |other| return utils.printErr(
            error.UnexpectedExpression,
            "Parser: Expected struct name in struct instantiation, received {s}.",
            .{@tagName(other)},
            .red,
        ),
    };

    try self.expect(self.advance(), Lexer.Token.open_brace, "struct instantiation", "{");

    var @"struct" = ast.Expression.StructInstantiation{
        .name = struct_name,
        .members = .init(self.alloc),
    };

    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        const member_name = try self.expect(self.advance(), Lexer.Token.ident, "struct instantiation", "struct member name");
        try self.expect(self.advance(), Lexer.Token.colon, "struct instantiation", ":");
        const member_value = try parse(self, .default);

        try @"struct".members.put(member_name, member_value);

        if (self.currentTokenKind() != Lexer.Token.close_brace) {
            try self.expect(self.advance(), Lexer.Token.comma, "struct instantiation", ",");
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, "struct instantiation", "}");

    return self.putExprPos(
        .{ .struct_instantiation = @"struct" },
        try self.getExprPos(lhs.*),
    );
}

pub fn arrayInstantiation(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();

    try self.expect(self.advance(), Lexer.Token.open_bracket, "array instantiation", "[");
    const length = try self.alloc.create(ast.Expression);
    length.* = try parse(self, .default);
    try self.expect(self.advance(), Lexer.Token.close_bracket, "array instantiation", "]");

    var array: ast.Expression.ArrayInstantiation = .{
        .type = try self.type_parser.parseType(self.alloc, .default),
        .length = length,
    };

    try self.expect(self.advance(), Lexer.Token.open_brace, "array instantiation", "{");
    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        try array.contents.append(self.alloc, try parse(self, .logical));

        if (self.currentTokenKind() != Lexer.Token.close_brace)
            try self.expect(self.advance(), Lexer.Token.comma, "array literal", ",");
    }
    try self.expect(self.advance(), Lexer.Token.close_brace, "array instantiation", "}");

    return self.putExprPos(.{ .array_instantiation = array }, pos);
}

pub fn call(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    return self.putExprPos(.{
        .call = .{
            .callee = lhs,
            .args = try self.parseArguments(),
        },
    }, try self.getExprPos(lhs.*));
}

pub fn @"if"(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();
    try self.expect(self.advance(), Lexer.Token.@"if", "if expression", "if");

    try self.expect(self.advance(), Lexer.Token.open_paren, "if expression", "(");

    const condition = try self.alloc.create(ast.Expression);
    condition.* = try parse(self, .default);

    try self.expect(self.advance(), Lexer.Token.close_paren, "if expression", ")");

    const capture: ?[]const u8 = switch (self.currentToken()) {
        .pipe => blk: {
            _ = self.advance(); // consume opening pipe
            const capture_name = try self.expect(self.advance(), Lexer.Token.ident, "capture", "capture name");
            try self.expect(self.advance(), Lexer.Token.pipe, "capture", "|"); // consume closing pipe
            break :blk capture_name;
        },
        else => null,
    };

    const body = try self.alloc.create(ast.Expression);
    body.* = if (self.currentTokenKind() == .open_brace)
        try block(self)
    else
        try parse(self, .default);

    var @"else": ?*ast.Expression = null;
    if (self.currentTokenKind() == Lexer.Token.@"else") {
        _ = self.advance(); // consume `else`

        @"else" = try self.alloc.create(ast.Expression);
        @"else".?.* = if (self.currentTokenKind() == .open_brace)
            try block(self)
        else
            try parse(self, .default);
    }

    return self.putExprPos(.{
        .@"if" = .{
            .condition = condition,
            .capture = capture,
            .body = body,
            .@"else" = @"else",
        },
    }, pos);
}

pub fn block(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();
    return self.putExprPos(.{ .block = try self.parseBlock() }, pos);
}

pub fn range(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    const token = self.advance(); // move past '..|..='

    const end = try self.alloc.create(ast.Expression);
    end.* = try parse(self, .default);

    return self.putExprPos(.{
        .range = .{
            .start = lhs,
            .end = end,
            .inclusive = token != .dot_dot,
        },
    }, try self.getExprPos(lhs.*));
}

pub fn index(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    _ = self.advance(); // move past '['

    const i = try self.alloc.create(ast.Expression);
    i.* = try parse(self, .default);

    try self.expect(self.advance(), .close_bracket, "index expression", "]");

    return self.putExprPos(.{
        .index = .{
            .lhs = lhs,
            .index = i,
        },
    }, try self.getExprPos(lhs.*));
}

pub fn reference(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();
    _ = self.advance(); // consume `&`

    const is_mut = self.currentTokenKind() == Lexer.Token.mut;
    if (is_mut) _ = self.advance(); // consume `mut`

    const inner = try self.alloc.create(ast.Expression);
    inner.* = try parse(self, .default);

    return self.putExprPos(.{
        .reference = .{
            .inner = inner,
            .is_mut = is_mut,
        },
    }, pos);
}

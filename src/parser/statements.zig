const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");
const expressions = @import("expressions.zig");

const TypeParser = @import("TypeParser.zig");

const Parser = @import("parser.zig").Parser;
const Error = @import("parser.zig").Error;

pub fn parse(self: *Parser) Error!ast.Statement {
    if (self.statement_lookup.get(self.currentToken())) |statement_fn| {
        return statement_fn(self);
    }

    const expression = try expressions.parse(self, .default, .{});

    if (self.currentToken() == .@";") {
        _ = self.advance();
        return .{ .expression = expression };
    } else return .{ .block_eval = expression };
}

pub fn variableDefinition(self: *Parser) Error!ast.Statement {
    return try variableDeclarationGeneric(self, false);
}

pub fn constDefinition(self: *Parser) Error!ast.Statement {
    return try variableDeclarationGeneric(self, true);
}

fn variableDeclarationGeneric(self: *Parser, comptime is_const: bool) Error!ast.Statement {
    const env_small = if (is_const) "constant" else "variable";
    const environment = env_small ++ " declaration statement";

    const is_pub = self.isPub();

    const pos = self.pos;
    _ = self.advance(); // consume `let`

    const is_mut = self.currentToken() == .mut;
    if (!is_const and is_mut) _ = self.advance(); // consume `mut`

    const var_name = try self.expect(self.advance(), .ident, environment, env_small);

    // optionally parse type
    var @"type": ast.Type = .{ .inferred = .{ .pos = self.pos } };
    errdefer @"type".deinit(self.alloc);
    if (self.currentToken() == .@":") {
        _ = self.advance(); // consume @":"
        @"type" = try self.type_parser.parseType(self.alloc, .default);
    }

    try self.expect(self.advance(), .@"=", environment, "=");

    const assigned_value = try expressions.parse(self, .assignment, .{});
    errdefer assigned_value.deinit(self.alloc);

    try self.expectSemicolon(environment);

    return .{
        .variable_definition = .{
            .pos = pos,
            .is_pub = is_pub,
            .variable_name = try self.alloc.dupe(u8, var_name),
            .binding = if (is_const) .@"const" else if (is_mut) .let_mut else .let,
            .assigned_value = assigned_value,
            .type = @"type",
        },
    };
}

pub fn @"return"(self: *Parser) Error!ast.Statement {
    const pos = self.pos;
    _ = self.advance(); // consume "return" keyword and parse from there.

    var expression: ?ast.Expression = null;
    errdefer if (expression) |expr| expr.deinit(self.alloc);
    if (self.currentToken() != .@";")
        expression = try expressions.parse(self, .default, .{});

    try self.expectSemicolon("return statement");
    return .{ .@"return" = .{ .pos = pos, .@"return" = expression } };
}

pub fn @"for"(self: *Parser) Error!ast.Statement {
    const pos = self.pos;
    _ = self.advance(); // consume "for" keyeword and parse from there.

    try self.expect(self.advance(), .@"(", "for statement iterator", "(");
    const iterator = try expressions.parse(self, .default, .{});
    errdefer iterator.deinit(self.alloc);
    try self.expect(self.advance(), .@")", "for statement iterator", ")");

    const capture = try self.parseCapture();
    errdefer if (capture) |c| c.deinit(self.alloc);

    const body = try self.alloc.create(ast.Statement);
    errdefer self.alloc.destroy(body);
    body.* = if (self.currentToken() == .@"{")
        .{ .block = .{ .pos = self.pos, .payload = try self.parseBlock() } }
    else
        try parse(self);

    return .{
        .@"for" = .{
            .pos = pos,
            .iterator = iterator,
            .capture = capture,
            .body = body,
        },
    };
}

pub fn @"while"(self: *Parser) Error!ast.Statement {
    return try conditional(self, .@"while");
}

pub fn @"if"(self: *Parser) Error!ast.Statement {
    return try conditional(self, .@"if");
}

/// parses either `if` statement or `while` statement
pub fn conditional(self: *Parser, comptime @"type": enum { @"if", @"while" }) Error!ast.Statement {
    const context = switch (@"type") {
        .@"while" => "while statement",
        .@"if" => "if statement",
    };

    _ = self.advance(); // consume `if` or `while` keyword

    try self.expect(self.advance(), .@"(", context, "(");

    const condition = try expressions.parse(self, .default, .{});
    errdefer condition.deinit(self.alloc);

    try self.expect(self.advance(), .@")", context, ")");

    const capture = try self.parseCapture();
    errdefer if (capture) |c| c.deinit(self.alloc);

    const body = try self.alloc.create(ast.Statement);
    errdefer self.alloc.destroy(body);
    body.* = if (self.currentToken() == .@"{")
        .{ .block = .{ .pos = self.pos, .payload = try self.parseBlock() } }
    else
        try parse(self);
    errdefer body.deinit(self.alloc);

    const pos = self.pos;
    return switch (@"type") {
        .@"if" => {
            var @"else": ?*ast.Statement = null;
            errdefer if (@"else") |e| e.deinitPtr(self.alloc);
            if (self.currentToken() == .@"else") {
                _ = self.advance(); // consume `else`

                @"else" = try self.alloc.create(ast.Statement);
                @"else".?.* = if (self.currentToken() == .@"{")
                    .{ .block = .{ .pos = self.pos, .payload = try self.parseBlock() } }
                else
                    try parse(self);
            }

            return .{
                .@"if" = .{
                    .pos = pos,
                    .condition = condition,
                    .capture = capture,
                    .body = body,
                    .@"else" = @"else",
                },
            };
        },
        .@"while" => .{
            .@"while" = .{
                .pos = pos,
                .condition = condition,
                .capture = capture,
                .body = body,
            },
        },
    };
}

pub fn match(self: *Parser) Error!ast.Statement {
    return .{ .expression = try expressions.parse(self, .primary, .{}) };
}

pub fn @"break"(self: *Parser) Error!ast.Statement {
    const pos = self.pos;
    _ = self.advance();
    try self.expectSemicolon("break statement");
    return .{ .@"break" = .{ .pos = pos } };
}

pub fn @"continue"(self: *Parser) Error!ast.Statement {
    const pos = self.pos;
    _ = self.advance();
    try self.expectSemicolon("continue statement");
    return .{ .@"continue" = .{ .pos = pos } };
}

pub fn @"defer"(self: *Parser) Error!ast.Statement {
    const pos = self.pos;
    _ = self.advance();
    const stmt = try self.alloc.create(ast.Statement);
    errdefer self.alloc.destroy(stmt);
    stmt.* = try parse(self);
    return .{ .@"defer" = .{ .pos = pos, .payload = stmt } };
}

pub fn @"pub"(self: *Parser) Error!ast.Statement {
    _ = self.advance(); // consume `pub` keyword
    switch (self.input[self.pos]) {
        .@"enum", .@"struct", .@"union", .@"fn", .bind, .import, .let, .@"const" => {},
        else => |t| return utils.printErr(
            error.UnexpectedToken,
            "Parser error: expected top level statement after 'pub', found '{f}' ({f}).\n",
            .{ t, self.source_map[self.pos] },
        ),
    }
    return parse(self);
}

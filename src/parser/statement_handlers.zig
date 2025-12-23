const std = @import("std");
const pretty = @import("pretty");
const utils = @import("../utils.zig");
const ast = @import("ast.zig");
const expression_handlers = @import("expression_handlers.zig");

const Lexer = @import("../Lexer.zig");
const TypeParser = @import("TypeParser.zig");

const Self = @import("Parser.zig");
const ParserError = Self.ParserError;

pub fn parseStatement(self: *Self) ParserError!ast.Statement {
    const pos = self.currentPosition();
    if (self.statement_lookup.get(self.currentTokenKind())) |statement_fn| {
        return statement_fn(self);
    }

    const expression = try expression_handlers.parseExpression(self, .default);

    try self.expect(self.currentToken(), Lexer.Token.semicolon, "statement", ";");
    _ = self.advance(); // consume semicolon

    return self.putStatementPos(.{ .expression = expression }, pos);
}

pub fn parseVariableDeclarationStatement(self: *Self) ParserError!ast.Statement {
    const pos = self.currentPosition();
    _ = self.advance(); // consume `let`

    const is_mut = self.currentTokenKind() == Lexer.Token.mut;
    if (is_mut) _ = self.advance(); // consume `mut`

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
        @"type" = try self.type_parser.parseType(self.alloc, .default);
    }

    try self.expect(
        self.advance(),
        Lexer.Token.equals,
        "variable declaration statement",
        "=",
    );

    const assigned_value = try expression_handlers.parseExpression(self, .assignment);

    try self.expect(
        self.advance(),
        Lexer.Token.semicolon,
        "variable declaration statement",
        ";",
    );

    return self.putStatementPos(.{
        .variable_definition = .{
            .variable_name = var_name,
            .is_mut = is_mut,
            .assigned_value = assigned_value,
            .type = @"type",
        },
    }, pos);
}

/// parses either a struct, enum, or union declaration statement
pub fn parseCompoundTypeDeclarationStatement(
    self: *Self,
    comptime @"type": enum { @"struct", @"enum", @"union" },
) ParserError!ast.Statement {
    const context = switch (@"type") {
        .@"struct" => "struct declaration statement",
        .@"enum" => "enum declaration statement",
        .@"union" => "union declaration statement",
    };

    const pos = self.currentPosition();

    _ = self.advance(); // consume `struct`, `enum`, or `union` keyword.

    var compound: ast.Statement = switch (@"type") {
        .@"struct" => .{ .struct_declaration = .{ .name = try self.expect(self.advance(), Lexer.Token.ident, context, "struct name") } },
        .@"enum" => .{ .enum_declaration = .{ .name = try self.expect(self.advance(), Lexer.Token.ident, context, "enum name") } },
        .@"union" => .{ .union_declaration = .{ .name = try self.expect(self.advance(), Lexer.Token.ident, context, "union name") } },
    };

    if (self.currentTokenKind() == Lexer.Token.open_paren)
        switch (@"type") {
            .@"struct" => compound.struct_declaration.generic_types = try self.parseGenericParameters(),
            .@"enum" => compound.enum_declaration.generic_types = try self.parseGenericParameters(),
            .@"union" => compound.union_declaration.generic_types = try self.parseGenericParameters(),
        };

    try self.expect(self.advance(), Lexer.Token.open_brace, context, "{");

    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        // parse member
        switch (self.currentToken()) {
            .ident => {
                const member_name = try self.expect(self.advance(), Lexer.Token.ident, context, "member name");

                const member_type: ?ast.Type =
                    switch (@"type") {
                        .@"struct", .@"union" => blk: {
                            try self.expect(self.advance(), Lexer.Token.colon, context, ":");
                            break :blk try self.type_parser.parseType(self.alloc, .default);
                        },
                        .@"enum" => null,
                    };

                const default_value: ?ast.Expression =
                    switch (@"type") {
                        .@"struct", .@"enum" => if (self.currentTokenKind() == Lexer.Token.equals) blk: {
                            _ = self.advance();
                            break :blk try expression_handlers.parseExpression(self, .default);
                        } else null,
                        .@"union" => null,
                    };

                try switch (@"type") {
                    .@"struct" => compound.struct_declaration.members.append(self.alloc, .{
                        .name = member_name,
                        .type = member_type orelse unreachable,
                        .default_value = default_value,
                    }),
                    .@"enum" => compound.enum_declaration.members.append(self.alloc, .{
                        .name = member_name,
                        .default_value = default_value,
                    }),
                    .@"union" => compound.union_declaration.members.append(self.alloc, .{
                        .name = member_name,
                        .type = member_type,
                    }),
                };

                self.expectSilent(self.currentToken(), Lexer.Token.comma) catch break;
                _ = self.advance(); // if there was a comma, consume it
            },
            .@"fn" => {
                const @"fn" = (try parseFunctionDefinition(self)).function_definition;

                try switch (@"type") {
                    .@"struct" => compound.struct_declaration.methods.append(self.alloc, @"fn"),
                    .@"enum" => compound.enum_declaration.methods.append(self.alloc, @"fn"),
                    .@"union" => compound.union_declaration.methods.append(self.alloc, @"fn"),
                };
            },

            else => unreachable,
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, context, "}");

    return self.putStatementPos(compound, pos);
}

pub fn parseStructDeclarationStatement(self: *Self) ParserError!ast.Statement {
    return try parseCompoundTypeDeclarationStatement(self, .@"struct");
}

pub fn parseEnumDeclarationStatement(self: *Self) ParserError!ast.Statement {
    return try parseCompoundTypeDeclarationStatement(self, .@"enum");
}

pub fn parseUnionDeclarationStatement(self: *Self) ParserError!ast.Statement {
    return try parseCompoundTypeDeclarationStatement(self, .@"union");
}

pub fn parseFunctionDefinition(self: *Self) ParserError!ast.Statement {
    const pos = self.currentPosition();
    try self.expect(self.advance(), Lexer.Token.@"fn", "function definition", "fn");
    const function_name = try self.expect(self.advance(), Lexer.Token.ident, "function definition", "function name");
    const parameters = try self.parseParameters();
    const return_type = try self.type_parser.parseType(self.alloc, .default);
    const body = try self.parseBlock();

    return self.putStatementPos(.{
        .function_definition = .{
            .name = function_name,
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
        },
    }, pos);
}

pub fn parseReturnStatement(self: *Self) ParserError!ast.Statement {
    _ = self.advance(); // consume "return" keyword and parse from there.

    const pos = self.currentPosition();
    var expression: ?ast.Expression = null;
    if (self.currentTokenKind() != Lexer.Token.semicolon)
        expression = try expression_handlers.parseExpression(self, .default);

    try self.expect(self.advance(), Lexer.Token.semicolon, "return statement", ";");
    return self.putStatementPos(.{
        .@"return" = expression,
    }, pos);
}

pub fn parseForStatement(self: *Self) ParserError!ast.Statement {
    const pos = self.currentPosition();
    _ = self.advance(); // consume "for" keyeword and parse from there.

    try self.expect(self.advance(), Lexer.Token.open_paren, "for statement iterator", "(");
    const iterator = try self.alloc.create(ast.Expression);
    iterator.* = try expression_handlers.parseExpression(self, .default);
    try self.expect(self.advance(), Lexer.Token.close_paren, "for statement iterator", ")");

    try self.expect(self.advance(), Lexer.Token.pipe, "for statement capture", "|");
    const capture = try self.expect(self.advance(), Lexer.Token.ident, "for statement capture", "for statement capture identifier");
    try self.expect(self.advance(), Lexer.Token.pipe, "for statement capture", "|");

    const body = try self.alloc.create(ast.Statement);
    body.* = if (self.currentTokenKind() == .open_brace)
        .{ .block = try self.parseBlock() }
    else
        try parseStatement(self);

    return self.putStatementPos(.{
        .@"for" = .{
            .iterator = iterator,
            .capture = capture,
            .body = body,
        },
    }, pos);
}

pub fn parseWhileStatement(self: *Self) ParserError!ast.Statement {
    return try parseConditionalStatement(self, .@"while");
}

pub fn parseIfStatement(self: *Self) ParserError!ast.Statement {
    return try parseConditionalStatement(self, .@"if");
}

/// parses either `if` statement or `while` statement
pub fn parseConditionalStatement(self: *Self, comptime @"type": enum { @"if", @"while" }) ParserError!ast.Statement {
    const context = switch (@"type") {
        .@"while" => "while statement",
        .@"if" => "if statement",
    };

    _ = self.advance(); // consume `if` or `while` keyword

    try self.expect(self.advance(), Lexer.Token.open_paren, context, "(");

    const condition = try self.alloc.create(ast.Expression);
    condition.* = try expression_handlers.parseExpression(self, .default);

    try self.expect(self.advance(), Lexer.Token.close_paren, context, ")");

    const capture: ?[]const u8 = switch (self.currentToken()) {
        .pipe => blk: {
            _ = self.advance(); // consume opening pipe
            const capture_name = try self.expect(self.advance(), Lexer.Token.ident, "capture", "capture name");
            try self.expect(self.advance(), Lexer.Token.pipe, "capture", "|"); // consume closing pipe
            break :blk capture_name;
        },
        else => null,
    };

    const body = try self.alloc.create(ast.Statement);
    body.* = if (self.currentTokenKind() == .open_brace)
        .{ .block = try self.parseBlock() }
    else
        try parseStatement(self);

    const pos = self.currentPosition();
    return switch (@"type") {
        .@"if" => {
            var @"else": ?*ast.Statement = null;
            if (self.currentTokenKind() == Lexer.Token.@"else") {
                _ = self.advance(); // consume `else`

                @"else" = try self.alloc.create(ast.Statement);
                @"else".?.* = if (self.currentTokenKind() == .open_brace)
                    .{ .block = try self.parseBlock() }
                else
                    try parseStatement(self);
            }

            return self.putStatementPos(.{
                .@"if" = .{
                    .condition = condition,
                    .capture = capture,
                    .body = body,
                    .@"else" = @"else",
                },
            }, pos);
        },
        .@"while" => self.putStatementPos(.{
            .@"while" = .{
                .condition = condition,
                .capture = capture,
                .body = body,
            },
        }, pos),
    };
}

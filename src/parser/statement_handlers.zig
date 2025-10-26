const std = @import("std");
const pretty = @import("pretty");
const utils = @import("../utils.zig");
const ast = @import("ast.zig");
const expression_handlers = @import("expression_handlers.zig");

const Lexer = @import("../Lexer.zig");
const TypeParser = @import("TypeParser.zig");

const Self = @import("Parser.zig");
const ParserError = Self.ParserError;

pub fn parseStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    if (self.statement_lookup.get(self.currentTokenKind())) |statement_fn| {
        return statement_fn(self, alloc);
    }

    const expression = try expression_handlers.parseExpression(self, alloc, .default);

    try self.expect(self.currentToken(), Lexer.Token.semicolon, "statement", ";");
    _ = self.advance(); // consume semicolon

    return .{ .expression = expression };
}

pub fn parseVariableDeclarationStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
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
        @"type" = try self.type_parser.parseType(alloc, .default);
    }

    try self.expect(
        self.advance(),
        Lexer.Token.equals,
        "variable declaration statement",
        "=",
    );

    const assigned_value = try expression_handlers.parseExpression(self, alloc, .assignment);

    try self.expect(
        self.advance(),
        Lexer.Token.semicolon,
        "variable declaration statement",
        ";",
    );

    return .{
        .variable_declaration = .{
            .variable_name = var_name,
            .is_mut = is_mut,
            .assigned_value = assigned_value,
            .type = @"type",
        },
    };
}

pub fn parseStructDeclarationStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    try self.expect(self.advance(), Lexer.Token.@"struct", "struct declaration", "struct");

    var @"struct" = ast.Statement.StructDeclaration{
        .name = try self.expect(self.advance(), Lexer.Token.ident, "struct declaration", "struct name"),
    };

    if (self.currentTokenKind() == Lexer.Token.open_paren)
        @"struct".generic_types = try self.parseGenericParameters(alloc);

    try self.expect(self.advance(), Lexer.Token.open_brace, "struct declaration", "{");

    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        // parse member
        switch (self.currentToken()) {
            .ident => {
                const member_name = try self.expect(self.advance(), Lexer.Token.ident, "struct declaration", "member name");
                try self.expect(self.advance(), Lexer.Token.colon, "struct declaration", ":");
                const member_type = try self.type_parser.parseType(alloc, .default);

                var default_value: ?ast.Expression = null;

                if (self.currentTokenKind() == Lexer.Token.equals) {
                    _ = self.advance();
                    default_value = try expression_handlers.parseExpression(self, alloc, .default);
                }

                try @"struct".members.append(alloc, .{
                    .name = member_name,
                    .type = member_type,
                    .default_value = default_value,
                });

                self.expectSilent(self.currentToken(), Lexer.Token.comma) catch break;
                _ = self.advance(); // if there was a comma, consume it
            },
            .@"fn" => try @"struct".methods.append(
                alloc,
                (try parseFunctionDefinition(self, alloc)).function_definition,
            ),
            else => unreachable,
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, "struct declaration", "}");

    return .{ .struct_declaration = @"struct" };
}

pub fn parseEnumDeclarationStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    try self.expect(self.advance(), Lexer.Token.@"enum", "enum declaration", "enum");

    var @"enum" = ast.Statement.EnumDeclaration{
        .name = try self.expect(self.advance(), Lexer.Token.ident, "enum declaration", "enum name"),
    };

    if (self.currentTokenKind() == Lexer.Token.open_paren)
        @"enum".generic_types = try self.parseGenericParameters(alloc);

    try self.expect(self.advance(), Lexer.Token.open_brace, "enum declaration", "{");

    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        // parse member
        switch (self.currentToken()) {
            .ident => {
                const member_name = try self.expect(self.advance(), Lexer.Token.ident, "enum declaration", "member name");
                var default_value: ?ast.Expression = null;

                if (self.currentTokenKind() == Lexer.Token.equals) {
                    _ = self.advance();
                    default_value = try expression_handlers.parseExpression(self, alloc, .default);
                }

                try @"enum".members.append(alloc, .{
                    .name = member_name,
                    .default_value = default_value,
                });

                self.expectSilent(self.currentToken(), Lexer.Token.comma) catch break;
                _ = self.advance(); // if there was a comma, consume it
            },
            .@"fn" => try @"enum".methods.append(
                alloc,
                (try parseFunctionDefinition(self, alloc)).function_definition,
            ),
            else => unreachable,
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, "enum declaration", "}");

    return .{ .enum_declaration = @"enum" };
}

pub fn parseUnionDeclarationStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    try self.expect(self.advance(), Lexer.Token.@"union", "union declaration", "union");

    var @"union" = ast.Statement.UnionDeclaration{
        .name = try self.expect(self.advance(), Lexer.Token.ident, "union declaration", "union name"),
    };

    if (self.currentTokenKind() == Lexer.Token.open_paren)
        @"union".generic_types = try self.parseGenericParameters(alloc);

    try self.expect(self.advance(), Lexer.Token.open_brace, "union declaration", "{");

    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        // parse member
        switch (self.currentToken()) {
            .ident => {
                const member_name = try self.expect(self.advance(), Lexer.Token.ident, "union declaration", "member name");
                var member_type: ?ast.Type = null;

                if (self.currentTokenKind() == Lexer.Token.colon) {
                    _ = self.advance();
                    member_type = try self.type_parser.parseType(alloc, .default);
                }

                try @"union".members.append(alloc, .{
                    .name = member_name,
                    .type = member_type,
                });

                self.expectSilent(self.currentToken(), Lexer.Token.comma) catch break;
                _ = self.advance(); // if there was a comma, consume it
            },
            .@"fn" => try @"union".methods.append(
                alloc,
                (try parseFunctionDefinition(self, alloc)).function_definition,
            ),
            else => unreachable,
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, "union declaration", "}");

    return .{ .union_declaration = @"union" };
}

pub fn parseFunctionDefinition(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
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

pub fn parseWhileStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    try self.expect(self.advance(), Lexer.Token.@"while", "while statement", "while");

    try self.expect(self.advance(), Lexer.Token.open_paren, "while statement", "(");

    const condition = try alloc.create(ast.Expression);
    condition.* = try expression_handlers.parseExpression(self, alloc, .default);

    try self.expect(self.advance(), Lexer.Token.close_paren, "while statement", ")");

    const capture: ?[]const u8 = switch (self.currentToken()) {
        .pipe => blk: {
            _ = self.advance(); // consume opening pipe
            const capture_name = try self.expect(self.advance(), Lexer.Token.ident, "capture", "capture name");
            try self.expect(self.advance(), Lexer.Token.pipe, "capture", "|"); // consume closing pipe
            break :blk capture_name;
        },
        else => null,
    };

    const body = try alloc.create(ast.Statement);
    body.* = if (self.currentTokenKind() == .open_brace)
        .{ .block = try self.parseBlock(alloc) }
    else
        try parseStatement(self, alloc);

    return .{
        .@"while" = .{
            .condition = condition,
            .capture = capture,
            .body = body,
        },
    };
}

pub fn parseReturnStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    _ = self.advance(); // consume "return" keyword and parse from there.

    var expression: ?ast.Expression = null;
    if (self.currentTokenKind() != Lexer.Token.semicolon)
        expression = try expression_handlers.parseExpression(self, alloc, .default);

    try self.expect(self.advance(), Lexer.Token.semicolon, "return statement", ";");
    return .{ .@"return" = expression };
}

pub fn parseForStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    _ = self.advance(); // consume "for "keyeword and parse from there.

    try self.expect(self.advance(), Lexer.Token.open_paren, "for statement iterator", "(");
    const iterator = try alloc.create(ast.Expression);
    iterator.* = try expression_handlers.parseExpression(self, alloc, .default);
    try self.expect(self.advance(), Lexer.Token.close_paren, "for statement iterator", ")");

    try self.expect(self.advance(), Lexer.Token.pipe, "for statement capture", "|");
    const capture = try self.expect(self.advance(), Lexer.Token.ident, "for statement capture", "for statement capture identifier");
    try self.expect(self.advance(), Lexer.Token.pipe, "for statement capture", "|");

    const body = try alloc.create(ast.Expression);
    body.* = try expression_handlers.parseBlockExpression(self, alloc);

    return .{
        .@"for" = .{
            .iterator = iterator,
            .capture = capture,
            .body = body,
        },
    };
}

pub fn parseIfStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    try self.expect(self.advance(), Lexer.Token.@"if", "if expression", "if");

    try self.expect(self.advance(), Lexer.Token.open_paren, "if expression", "(");

    const condition = try alloc.create(ast.Expression);
    condition.* = try expression_handlers.parseExpression(self, alloc, .default);

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

    const body = try alloc.create(ast.Statement);
    body.* = if (self.currentTokenKind() == .open_brace)
        .{ .block = try self.parseBlock(alloc) }
    else
        try parseStatement(self, alloc);

    var @"else": ?*ast.Statement = null;
    if (self.currentTokenKind() == Lexer.Token.@"else") {
        _ = self.advance(); // consume `else`

        @"else" = try alloc.create(ast.Statement);
        @"else".?.* = if (self.currentTokenKind() == .open_brace)
            .{ .block = try self.parseBlock(alloc) }
        else
            try parseStatement(self, alloc);
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

const std = @import("std");
const pretty = @import("pretty");
const utils = @import("../utils.zig");
const ast = @import("ast.zig");
const expression_handlers = @import("expression_handlers.zig");

const Lexer = @import("../Lexer.zig");
const TypeParser = @import("TypeParser.zig");

const Self = @import("Parser.zig");
const ParserError = Self.ParserError;

fn ifEndsWithBlock(if_expr: ast.IfExpression) bool {
    if (if_expr.@"else") |else_expr| {
        return switch (else_expr.*) {
            .block => true,
            .@"if" => |inner_if| ifEndsWithBlock(inner_if),
            else => false,
        };
    }
    return if_expr.body.* == .block;
}

pub fn parseStatement(
    self: *Self,
    alloc: std.mem.Allocator,
    config: struct {
        silent: bool = false,
        require_semicolon: bool = true,
    },
) ParserError!ast.Statement {
    if (self.statement_lookup.get(self.currentTokenKind())) |statement_fn| {
        return statement_fn(self, alloc);
    }

    const expression = try expression_handlers.parseExpression(self, alloc, .default);

    var semicolon_required = config.require_semicolon;
    if (semicolon_required) {
        switch (expression) {
            .block => semicolon_required = false,
            .@"if" => |if_expr| if (ifEndsWithBlock(if_expr)) {
                semicolon_required = false;
            },
            else => {},
        }
    }

    if (semicolon_required) {
        if (config.silent) {
            try self.expectSilent(self.currentToken(), Lexer.Token.semicolon);
        } else {
            try self.expect(self.currentToken(), Lexer.Token.semicolon, "statement", ";");
        }
        _ = self.advance(); // consume semicolon
    } else {
        // Semicolon is optional, but if it exists, consume it.
        if (self.currentTokenKind() == .semicolon) {
            _ = self.advance();
        }
    }

    return .{ .expression = expression };
}

pub fn parseVariableDeclarationStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
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
                (try parseFunctionDefinition(self, alloc)).function_definition,
            ),
            else => unreachable,
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, "struct declaration", "}");

    return @"struct";
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

    const body = try alloc.create(ast.Expression);
    body.* = if (self.currentTokenKind() == .open_brace)
        try expression_handlers.parseBlockExpression(self, alloc)
    else
        try expression_handlers.parseExpression(self, alloc, .default);

    return .{
        .@"while" = .{
            .condition = condition,
            .capture = capture,
            .body = body,
        },
    };
}

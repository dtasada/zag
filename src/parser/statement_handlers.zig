const std = @import("std");
const pretty = @import("pretty");
const utils = @import("../utils.zig");
const ast = @import("ast.zig");
const expression_handlers = @import("expression_handlers.zig");

const Lexer = @import("../Lexer.zig");
const TypeParser = @import("TypeParser.zig");

const Self = @import("Parser.zig");
const ParserError = Self.ParserError;

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

    if (config.require_semicolon) {
        if (config.silent)
            try self.expectSilent(self.currentToken(), Lexer.Token.semicolon)
        else
            try self.expect(self.currentToken(), Lexer.Token.semicolon, "statement", ";");

        _ = self.advance(); // consume semicolon
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

pub fn parseIfStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    return .{ .expression = try expression_handlers.parseIfExpression(self, alloc) };
}

pub fn parseWhileStatement(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Statement {
    try self.expect(self.advance(), Lexer.Token.@"while", "while statement", "while");

    try self.expect(self.advance(), Lexer.Token.open_paren, "while statement", "(");

    const condition = try alloc.create(ast.Expression);
    condition.* = try expression_handlers.parseExpression(self, alloc, .default);

    try self.expect(self.advance(), Lexer.Token.close_paren, "while statement", ")");

    const capture: ?[]const u8 = switch (self.currentToken()) {
        .pipe => blk: {
            std.debug.print("getting capture", .{});
            _ = self.advance(); // consume opening pipe
            const capture_name = try self.expect(self.advance(), Lexer.Token.ident, "capture", "capture name");
            try self.expect(self.advance(), Lexer.Token.pipe, "capture", "|"); // consume closing pipe
            break :blk capture_name;
        },
        else => null,
    };

    var body = ast.Block{};
    const position_backup = self.pos; // save position bc we're testing parse statement.
    if (parseStatement(self, alloc, .{ .silent = true }) catch null) |body_statement| {
        try body.append(alloc, body_statement);
    } else {
        // if body isn't a statement, it must be a block. reset self.pos back to its original position
        self.pos = position_backup;
        body = try self.parseBlock(alloc);
    }

    return .{
        .@"while" = .{
            .condition = condition,
            .capture = capture,
            .body = body,
        },
    };
}

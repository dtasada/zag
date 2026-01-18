const std = @import("std");
const pretty = @import("pretty");
const utils = @import("utils");
const ast = @import("ast.zig");
const expressions = @import("expressions.zig");

const Lexer = @import("Lexer");
const TypeParser = @import("TypeParser.zig");

const Self = @import("Parser.zig");
const ParserError = Self.ParserError;

pub fn parse(self: *Self) ParserError!ast.Statement {
    if (self.statement_lookup.get(self.currentTokenKind())) |statement_fn| {
        return statement_fn(self);
    }

    const expression = try expressions.parse(self, .default);

    try self.expect(self.advance(), Lexer.Token.semicolon, "statement", ";");

    return .{ .expression = expression };
}

pub fn variableDeclaration(self: *Self) ParserError!ast.Statement {
    const is_pub = isPub(self);

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
    var @"type": ast.Type = .{ .inferred = .{ .position = self.currentPosition() } };
    if (self.currentTokenKind() == Lexer.Token.colon) {
        _ = self.advance(); // consume colon
        @"type" = try self.type_parser.parseType(self.alloc, .default);
    }

    try self.expect(self.advance(), .@"=", "variable declaration statement", "=");

    const assigned_value = try expressions.parse(self, .assignment);

    try self.expect(
        self.advance(),
        Lexer.Token.semicolon,
        "variable declaration statement",
        ";",
    );

    return .{
        .variable_definition = .{
            .pos = pos,
            .is_pub = is_pub,
            .variable_name = var_name,
            .is_mut = is_mut,
            .assigned_value = assigned_value,
            .type = @"type",
        },
    };
}

/// parses either a struct, enum, or union declaration statement
pub fn compoundTypeDeclaration(
    self: *Self,
    comptime T: enum { @"struct", @"enum", @"union" },
) ParserError!ast.Statement {
    const context = switch (T) {
        .@"struct" => "struct declaration statement",
        .@"enum" => "enum declaration statement",
        .@"union" => "union declaration statement",
    };

    const pos = self.currentPosition();

    const is_pub = isPub(self);

    _ = self.advance(); // consume `struct`, `enum`, or `union` keyword.

    var compound: switch (T) {
        .@"struct" => ast.Statement.StructDeclaration,
        .@"enum" => ast.Statement.EnumDeclaration,
        .@"union" => ast.Statement.UnionDeclaration,
    } = .{
        .pos = pos,
        .is_pub = is_pub,
        .name = try self.expect(
            self.advance(),
            Lexer.Token.ident,
            context,
            @tagName(T) ++ "struct name",
        ),
    };

    if (self.currentTokenKind() == Lexer.Token.@"<") {
        switch (T) {
            inline .@"struct", .@"union" => compound.generic_types = try self.parseGenericParameters(),
            .@"enum" => return utils.printErr(
                error.UnexpectedToken,
                "Parser error: enum declaration can't have generic parameters ({f}).\n",
                .{self.lexer.source_map.items[self.pos]},
                .red,
            ),
        }
    }

    try self.expect(self.advance(), Lexer.Token.open_brace, context, "{' or '<");

    while (self.currentToken() != .eof and self.currentTokenKind() != Lexer.Token.close_brace) {
        // parse member
        switch (self.currentToken()) {
            .ident => {
                const member_name = try self.expect(self.advance(), Lexer.Token.ident, context, "member name");

                const member_type: ?ast.Type =
                    switch (T) {
                        .@"struct", .@"union" => blk: {
                            try self.expect(self.advance(), Lexer.Token.colon, context, ":");
                            break :blk try self.type_parser.parseType(self.alloc, .default);
                        },
                        .@"enum" => null,
                    };

                const default_value: ?ast.Expression =
                    switch (T) {
                        .@"struct", .@"enum" => if (self.currentTokenKind() == .@"=") blk: {
                            _ = self.advance();
                            break :blk try expressions.parse(self, .default);
                        } else null,
                        .@"union" => null,
                    };

                try switch (T) {
                    .@"struct" => compound.members.append(self.alloc, .{
                        .name = member_name,
                        .type = member_type orelse unreachable,
                        .default_value = default_value,
                    }),
                    .@"enum" => compound.members.append(self.alloc, .{
                        .name = member_name,
                        .value = default_value,
                    }),
                    .@"union" => compound.members.append(self.alloc, .{
                        .name = member_name,
                        .type = member_type,
                    }),
                };

                self.expectSilent(self.currentToken(), Lexer.Token.comma) catch break;
                _ = self.advance(); // if there was a comma, consume it
            },
            .@"fn" => try compound.methods.append(
                self.alloc,
                (try functionDefinition(self)).function_definition,
            ),

            else => unreachable,
        }
    }

    try self.expect(self.advance(), Lexer.Token.close_brace, context, "}");

    return switch (T) {
        .@"struct" => .{ .struct_declaration = compound },
        .@"enum" => .{ .enum_declaration = compound },
        .@"union" => .{ .union_declaration = compound },
    };
}

pub fn structDeclaration(self: *Self) ParserError!ast.Statement {
    return try compoundTypeDeclaration(self, .@"struct");
}

pub fn enumDeclaration(self: *Self) ParserError!ast.Statement {
    return try compoundTypeDeclaration(self, .@"enum");
}

pub fn unionDeclaration(self: *Self) ParserError!ast.Statement {
    return try compoundTypeDeclaration(self, .@"union");
}

pub fn functionDefinition(self: *Self) ParserError!ast.Statement {
    const is_pub = isPub(self);

    const pos = self.currentPosition();
    _ = self.advance(); // consume "fn" keyword
    const function_name = try self.expect(self.advance(), Lexer.Token.ident, "function definition", "function name");
    const generic_parameters = switch (self.currentToken()) {
        .open_paren => null,
        .@"<" => try self.parseGenericParameters(),
        else => |other| return self.unexpectedToken("Function definition", "(' or '<", other),
    };
    const parameters = try self.parseParameters();
    const return_type = self.type_parser.parseType(self.alloc, .default) catch |err| switch (err) {
        error.HandlerDoesNotExist => return utils.printErr(
            error.MissingReturnType,
            "Parser error: missing return type in function '{s}' at {f}.\n",
            .{ function_name, self.currentPosition() },
            .red,
        ),
        else => return err,
    };
    const body = try self.parseBlock();

    return .{
        .function_definition = .{
            .pos = pos,
            .is_pub = is_pub,
            .name = function_name,
            .generic_parameters = generic_parameters,
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
        },
    };
}

pub fn bindingFunctionDeclaration(self: *Self) ParserError!ast.Statement {
    const is_pub = isPub(self);

    const pos = self.currentPosition();
    _ = self.advance(); // consume "bind" keyword
    try self.expect(self.advance(), Lexer.Token.@"fn", "binding function declaration", "fn");
    const function_name = try self.expect(self.advance(), Lexer.Token.ident, "binding function declaration", "function name");
    const generic_parameters = switch (self.currentToken()) {
        .open_paren => null,
        .@"<" => try self.parseGenericParameters(),
        else => |other| return self.unexpectedToken("Function definition", "(' or '<", other),
    };
    const parameters = try self.parseParameters();
    const return_type = self.type_parser.parseType(self.alloc, .default) catch |err| switch (err) {
        error.HandlerDoesNotExist => return utils.printErr(
            error.MissingReturnType,
            "Parser error: missing return type in function '{s}' at {f}.\n",
            .{ function_name, self.currentPosition() },
            .red,
        ),
        else => return err,
    };
    try self.expect(self.advance(), Lexer.Token.semicolon, "binding function definition", ";");

    return .{
        .binding_function_declaration = .{
            .pos = pos,
            .is_pub = is_pub,
            .name = function_name,
            .generic_parameters = generic_parameters,
            .parameters = parameters,
            .return_type = return_type,
        },
    };
}

pub fn @"return"(self: *Self) ParserError!ast.Statement {
    const pos = self.currentPosition();
    _ = self.advance(); // consume "return" keyword and parse from there.

    var expression: ?ast.Expression = null;
    if (self.currentTokenKind() != Lexer.Token.semicolon)
        expression = try expressions.parse(self, .default);

    try self.expect(self.advance(), Lexer.Token.semicolon, "return statement", ";");
    return .{ .@"return" = .{ .pos = pos, .@"return" = expression } };
}

pub fn @"for"(self: *Self) ParserError!ast.Statement {
    const pos = self.currentPosition();
    _ = self.advance(); // consume "for" keyeword and parse from there.

    try self.expect(self.advance(), Lexer.Token.open_paren, "for statement iterator", "(");
    const iterator = try self.alloc.create(ast.Expression);
    iterator.* = try expressions.parse(self, .default);
    try self.expect(self.advance(), Lexer.Token.close_paren, "for statement iterator", ")");

    const capture = if (self.expect(self.advance(), .@"|", "for statement capture", "|") catch null) |_| b: {
        const capture = try self.expect(self.advance(), .ident, "for statement capture", "for statement capture identifier");
        try self.expect(self.advance(), .@"|", "for statement capture", "|");
        break :b capture;
    } else null;

    const body = try self.alloc.create(ast.Statement);
    body.* = if (self.currentTokenKind() == .open_brace)
        .{ .block = .{ .pos = self.currentPosition(), .block = try self.parseBlock() } }
    else
        try parse(self);

    return .{
        .@"for" = .{
            .pos = pos,
            .iterator = iterator,
            .capture = if (capture == null or std.mem.eql(u8, capture.?, "_")) null else capture,
            .body = body,
        },
    };
}

pub fn @"while"(self: *Self) ParserError!ast.Statement {
    return try conditional(self, .@"while");
}

pub fn @"if"(self: *Self) ParserError!ast.Statement {
    return try conditional(self, .@"if");
}

/// parses either `if` statement or `while` statement
pub fn conditional(self: *Self, comptime @"type": enum { @"if", @"while" }) ParserError!ast.Statement {
    const context = switch (@"type") {
        .@"while" => "while statement",
        .@"if" => "if statement",
    };

    _ = self.advance(); // consume `if` or `while` keyword

    try self.expect(self.advance(), Lexer.Token.open_paren, context, "(");

    const condition = try self.alloc.create(ast.Expression);
    condition.* = try expressions.parse(self, .default);

    try self.expect(self.advance(), Lexer.Token.close_paren, context, ")");

    const capture: ?[]const u8 = switch (self.currentToken()) {
        .@"|" => blk: {
            _ = self.advance(); // consume opening pipe
            const capture_name = try self.expect(self.advance(), Lexer.Token.ident, "capture", "capture name");
            try self.expect(self.advance(), .@"|", "capture", "|"); // consume closing pipe
            break :blk if (std.mem.eql(u8, capture_name, "_")) null else capture_name;
        },
        else => null,
    };

    const body = try self.alloc.create(ast.Statement);
    body.* = if (self.currentTokenKind() == .open_brace)
        .{ .block = .{ .pos = self.currentPosition(), .block = try self.parseBlock() } }
    else
        try parse(self);

    const pos = self.currentPosition();
    return switch (@"type") {
        .@"if" => {
            var @"else": ?*ast.Statement = null;
            if (self.currentTokenKind() == Lexer.Token.@"else") {
                _ = self.advance(); // consume `else`

                @"else" = try self.alloc.create(ast.Statement);
                @"else".?.* = if (self.currentTokenKind() == .open_brace)
                    .{ .block = .{ .pos = self.currentPosition(), .block = try self.parseBlock() } }
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

pub fn import(self: *Self) ParserError!ast.Statement {
    const pos = self.currentPosition();
    _ = self.advance(); // consume `import` keyword

    var module: std.ArrayList([]const u8) = .empty;
    var alias: ?[]const u8 = null;

    while (true) s: switch (self.currentToken()) {
        .ident => |ident| {
            try module.append(self.alloc, ident);
            _ = self.advance();
        },
        .@"." => continue :s self.advance(),
        .semicolon => break,
        .as => break,
        else => |other| return self.unexpectedToken("import statement", "as' or ';", other),
    };

    if (module.items.len == 0) return utils.printErr(
        error.SyntaxError,
        "Parser error: import statement must include a module identifier ({f}).",
        .{self.currentPosition()},
        .red,
    );

    switch (self.currentToken()) {
        .as => {
            _ = self.advance();
            alias = try self.expect(
                self.advance(),
                .ident,
                "import statement",
                "module alias",
            );
        },
        .semicolon => {},
        else => |other| return self.unexpectedToken("import statement", "as' or ';", other),
    }

    try self.expect(self.advance(), .semicolon, "import statement", ";");

    return .{
        .import = .{
            .pos = pos,
            .module_name = module,
            .alias = alias,
        },
    };
}

pub fn @"pub"(self: *Self) ParserError!ast.Statement {
    _ = self.advance(); // consume `pub` keyword
    return parse(self);
}

fn isPub(self: *Self) bool {
    return if (self.pos <= 0) false else self.previousToken() == .@"pub";
}

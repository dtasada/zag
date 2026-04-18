const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");
const expressions = @import("expressions.zig");
const statements = @import("statements.zig");

const TypeParser = @import("TypeParser.zig");

const parser = @import("parser.zig");
const Parser = parser.Parser;
const Error = parser.Error;

pub fn parse(self: *Parser) Error!ast.TopLevelStatement {
    if (self.top_level_statement_lookup.get(self.currentToken())) |statement_fn|
        return statement_fn(self);

    return utils.printErr(
        self.io,
        error.MustBeTopLevelStatement,
        "Parser error: found {s} but expected a top-level statement ({f}).\n",
        .{ @tagName(self.currentToken()), self.source_map[self.pos] },
    );
}

/// parses either a struct, enum, or union declaration statement
fn compoundTypeDeclaration(
    self: *Parser,
    comptime T: utils.CompoundTypeTag,
) Error!ast.TopLevelStatement {
    const context = @tagName(T) ++ " declaration statement";
    const tag_name = @tagName(T) ++ "_declaration";

    const pos = self.pos;
    const is_pub = self.isPub();

    _ = self.advance(); // consume `struct`, `enum`, or `union` keyword.

    const name = try self.expect(self.advance(), .ident, context, @tagName(T) ++ " name");

    const Type = switch (T) {
        .@"struct" => ast.TopLevelStatement.StructDeclaration,
        .@"union" => ast.TopLevelStatement.UnionDeclaration,
        .@"enum" => ast.TopLevelStatement.EnumDeclaration,
    };
    var compound: Type = switch (T) {
        .@"struct", .@"union" => .{
            .pos = pos,
            .is_pub = is_pub,
            .name = try self.alloc.dupe(u8, name),
            .generic_types = &.{},
            .variables = &.{},
            .subtypes = &.{},
            .members = &.{},
            .methods = &.{},
        },
        .@"enum" => .{
            .pos = pos,
            .is_pub = is_pub,
            .name = try self.alloc.dupe(u8, name),
            .variables = &.{},
            .subtypes = &.{},
            .members = &.{},
            .methods = &.{},
        },
    };

    var methods: std.ArrayList(ast.TopLevelStatement.FunctionDefinition) = .empty;
    var variables: std.ArrayList(ast.Statement.VariableDefinition) = .empty;
    var subtypes: std.ArrayList(ast.TopLevelStatement.Subtype) = .empty;
    var members: std.ArrayList(Type.Member) = .empty;

    errdefer utils.deinitArrayList(ast.TopLevelStatement.FunctionDefinition, &methods, self.alloc);
    errdefer utils.deinitArrayList(ast.Statement.VariableDefinition, &variables, self.alloc);
    errdefer utils.deinitArrayList(ast.TopLevelStatement.Subtype, &subtypes, self.alloc);
    errdefer utils.deinitArrayList(Type.Member, &members, self.alloc);

    if (self.currentToken() == .@"<") switch (T) {
        inline .@"struct", .@"union" => compound.generic_types = try self.parseGenericParameters(),
        .@"enum" => return utils.printErr(
            self.io,
            error.UnexpectedToken,
            "Parser error: enum declaration can't have generic parameters ({f}).\n",
            .{self.source_map[self.pos]},
        ),
    };

    try self.expect(self.advance(), .@"{", context, "{' or '<");

    while (self.currentToken() != .eof and self.currentToken() != .@"}") {
        switch (self.currentToken()) {
            .ident => {
                var member_names: std.ArrayList([]const u8) = .empty;
                defer member_names.deinit(self.alloc);

                const first_name = try self.expect(self.advance(), .ident, context, "member name");
                try member_names.append(self.alloc, first_name);

                if (T != .@"enum") while (self.currentToken() == .@",") {
                    _ = self.advance();
                    if (self.currentToken() == .@"}") break;
                    try member_names.append(self.alloc, try self.expect(
                        self.advance(),
                        .ident,
                        @tagName(T) ++ " member",
                        "member name",
                    ));
                };

                const member_type: ?ast.Type = switch (T) {
                    .@"struct" => b: {
                        try self.expect(self.advance(), .@":", context, ":");
                        break :b try self.type_parser.parseType(self.alloc, self.io, .default);
                    },
                    .@"union" => if (self.currentToken() == .@":") b: {
                        _ = self.advance();
                        break :b try self.type_parser.parseType(self.alloc, self.io, .default);
                    } else null,
                    .@"enum" => null,
                };
                defer if (member_type) |mt| mt.deinit(self.alloc);

                const value: ?ast.Expression = switch (T) {
                    .@"enum" => if (self.currentToken() == .@"=") b: {
                        _ = self.advance();
                        break :b try expressions.parse(self, .default, .{});
                    } else null,
                    else => null,
                };

                for (member_names.items) |i| try members.append(self.alloc, switch (T) {
                    .@"struct" => .{
                        .name = try self.alloc.dupe(u8, i),
                        .type = try member_type.?.clone(self.alloc),
                    },
                    .@"enum" => .{ .name = try self.alloc.dupe(u8, i), .value = value },
                    .@"union" => .{
                        .name = try self.alloc.dupe(u8, i),
                        .type = if (member_type) |mt| try mt.clone(self.alloc) else null,
                    },
                });

                if (self.currentToken() == .@",") _ = self.advance() else break;
            },
            .@"fn" => try methods.append(self.alloc, (try functionDefinition(self)).function_definition),
            .let, .@"const" => try variables.append(self.alloc, (try variableDefinition(self)).variable_definition),
            inline .@"struct", .@"union", .@"enum" => try subtypes.append(self.alloc, @unionInit(
                ast.TopLevelStatement.Subtype,
                @tagName(T),
                @field(try compoundTypeDeclaration(self, .@"struct"), tag_name),
            )),
            .@"pub" => _ = self.advance(),
            else => return utils.printErr(
                self.io,
                error.SyntaxError,
                "Parser error: expected {s} variable definition, member declaration or method definition in {s} '{s}' ({f}).\n",
                .{ @tagName(T), @tagName(T), name, self.source_map[self.pos] },
            ),
        }
    }

    try self.expect(self.advance(), .@"}", context, "}");

    compound.methods = try methods.toOwnedSlice(self.alloc);
    compound.variables = try variables.toOwnedSlice(self.alloc);
    compound.subtypes = try subtypes.toOwnedSlice(self.alloc);
    compound.members = try members.toOwnedSlice(self.alloc);

    return @unionInit(ast.TopLevelStatement, @tagName(T) ++ "_declaration", compound);
}

pub fn structDeclaration(self: *Parser) Error!ast.TopLevelStatement {
    return try compoundTypeDeclaration(self, .@"struct");
}

pub fn enumDeclaration(self: *Parser) Error!ast.TopLevelStatement {
    return try compoundTypeDeclaration(self, .@"enum");
}

pub fn unionDeclaration(self: *Parser) Error!ast.TopLevelStatement {
    return try compoundTypeDeclaration(self, .@"union");
}

pub fn functionDefinition(self: *Parser) Error!ast.TopLevelStatement {
    const is_pub = self.isPub();

    const pos = self.pos;
    _ = self.advance(); // consume "fn" keyword
    const function_name = try self.expect(self.advance(), .ident, "function definition", "function name");
    const generic_parameters: ast.ParameterList = switch (self.currentToken()) {
        .@"(" => &.{},
        .@"<" => try self.parseGenericParameters(),
        else => |other| return self.unexpectedToken("Function definition", "(' or '<", other),
    };
    errdefer utils.deinitSlice(ast.ParameterGroup, generic_parameters, self.alloc);

    const parameters = try self.parseParameters();
    errdefer utils.deinitSlice(ast.ParameterGroup, parameters, self.alloc);
    const return_type = self.type_parser.parseType(self.alloc, self.io, .default) catch |err| switch (err) {
        error.HandlerDoesNotExist, error.UnexpectedToken => return utils.printErr(
            self.io,
            error.MissingReturnType,
            "Parser error: missing return type in function '{s}' at {f}.\n",
            .{ function_name, self.source_map[self.pos] },
        ),
        else => return err,
    };
    errdefer return_type.deinit(self.alloc);

    const body: ast.Block = switch (self.currentToken()) {
        .@"{" => try self.parseBlock(),
        .@"->" => b: {
            _ = self.advance();
            const block = try self.alloc.alloc(ast.Statement, 1);
            errdefer self.alloc.free(block);
            block[0] = try statements.parse(self);
            break :b block;
        },
        else => return utils.printErr(
            self.io,
            error.UnexpectedToken,
            "Parser error: expected block after function definition type ({f}).\n",
            .{self.source_map[self.pos]},
        ),
    };
    errdefer utils.deinitSlice(ast.Statement, body, self.alloc);

    return .{
        .function_definition = .{
            .pos = pos,
            .is_pub = is_pub,
            .name = try self.alloc.dupe(u8, function_name),
            .generic_parameters = generic_parameters,
            .parameters = parameters,
            .return_type = return_type,
            .body = body,
        },
    };
}

pub fn bindingDeclaration(self: *Parser) Error!ast.TopLevelStatement {
    const is_pub = self.isPub();

    const pos = self.pos;
    _ = self.advance(); // consume "bind" keyword

    switch (self.advance()) {
        .@"fn" => {
            const function_name = try self.expect(self.advance(), .ident, "binding function declaration", "function name");
            const parameters = try self.parseParameters();
            errdefer utils.deinitSlice(ast.ParameterGroup, parameters, self.alloc);
            const return_type = self.type_parser.parseType(self.alloc, self.io, .default) catch |err| switch (err) {
                error.HandlerDoesNotExist => return utils.printErr(
                    self.io,
                    error.MissingReturnType,
                    "Parser error: missing return type in function '{s}' at {f}.\n",
                    .{ function_name, self.source_map[self.pos] },
                ),
                else => return err,
            };
            errdefer return_type.deinit(self.alloc);
            try self.expectSemicolon("binding function definition");

            return .{
                .binding_function_declaration = .{
                    .pos = pos,
                    .is_pub = is_pub,
                    .name = try self.alloc.dupe(u8, function_name),
                    .parameters = parameters,
                    .return_type = return_type,
                },
            };
        },
        inline .@"struct", .@"union", .@"enum" => |_, tag| {
            const type_decl: ast.TopLevelStatement = .{
                .binding_type_declaration = .{
                    .pos = pos,
                    .is_pub = is_pub,
                    .type = std.meta.stringToEnum(utils.CompoundTypeTag, @tagName(tag)).?,
                    .name = try self.alloc.dupe(u8, try self.expect(self.advance(), .ident, "binding type declaration", @tagName(tag) ++ " name")),
                },
            };
            errdefer type_decl.deinit(self.alloc);
            try self.expectSemicolon("binding type declaration");
            return type_decl;
        },
        else => |other| return utils.printErr(
            self.io,
            error.UnexpectedToken,
            "Parser error: expected function, struct, union or enum declaration after 'bind', received '{f}' ({f}).\n",
            .{ other, self.source_map[pos] },
        ),
    }
}

pub fn import(self: *Parser) Error!ast.TopLevelStatement {
    const pos = self.pos;
    _ = self.advance(); // consume `import` keyword

    var module: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (module.items) |name| self.alloc.free(name);
        module.deinit(self.alloc);
    }
    var alias: ?[]const u8 = null;

    while (true) s: switch (self.currentToken()) {
        .ident => |ident| {
            try module.append(self.alloc, try self.alloc.dupe(u8, ident));
            _ = self.advance();
        },
        .@"." => continue :s self.advance(),
        .@";" => break,
        .as => break,
        else => |other| return self.unexpectedToken("import statement", "as' or ';", other),
    };

    if (module.items.len == 0) return utils.printErr(
        self.io,
        error.SyntaxError,
        "Parser error: import statement must include a module identifier ({f}).",
        .{self.source_map[self.pos]},
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
        .@";" => {},
        else => |other| return self.unexpectedToken("import statement", "as' or ';", other),
    }

    try self.expectSemicolon("import statement");

    return .{
        .import = .{
            .pos = pos,
            .module_name = try module.toOwnedSlice(self.alloc),
            .alias = if (alias) |a| try self.alloc.dupe(u8, a) else null,
        },
    };
}

pub fn @"pub"(self: *Parser) Error!ast.TopLevelStatement {
    _ = self.advance(); // consume `pub` keyword
    switch (self.input[self.pos]) {
        .@"enum", .@"struct", .@"union", .@"fn", .bind, .import, .let, .@"const" => {},
        else => |t| return utils.printErr(
            self.io,
            error.UnexpectedToken,
            "Parser error: expected top level statement after 'pub', found '{f}' ({f}).\n",
            .{ t, self.source_map[self.pos] },
        ),
    }
    return parse(self);
}

pub fn variableDefinition(self: *Parser) Error!ast.TopLevelStatement {
    return .{
        .variable_definition = (try statements.variableDefinition(self)).variable_definition,
    };
}

pub fn constDefinition(self: *Parser) Error!ast.TopLevelStatement {
    return .{
        .variable_definition = (try statements.constDefinition(self)).variable_definition,
    };
}

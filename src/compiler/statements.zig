const std = @import("std");

const ast = @import("Parser").ast;
const expressions = @import("expressions.zig");

const Type = @import("Type.zig").Type;

const Self = @import("Compiler.zig");
const CompilerError = Self.CompilerError;

pub fn compileStatement(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    statement: *const ast.Statement,
) CompilerError!void {
    switch (statement.*) {
        .function_definition => |fn_def| try self.compileFunctionDefinition(file_writer, fn_def),
        .struct_declaration => |struct_decl| try compileCompoundTypeDeclaration(self, file_writer, .@"struct", struct_decl),
        .@"return" => |return_expr| try compileReturnStatement(self, file_writer, return_expr),
        .variable_definition => |var_decl| try compileVariableDefinition(self, file_writer, var_decl),
        .expression => |*expr| {
            try expressions.compileExpression(self, file_writer, expr);
            try self.write(file_writer, ";\n");
        },
        .@"if" => |if_stmt| try compileConditionalStatement(self, file_writer, .@"if", if_stmt),
        .@"while" => |while_stmt| try compileConditionalStatement(self, file_writer, .@"while", while_stmt),
        .@"for" => |for_stmt| try compileConditionalStatement(self, file_writer, .@"for", for_stmt),
        .block => |block| try self.compileBlock(file_writer, block),
        .enum_declaration => |enum_decl| try compileCompoundTypeDeclaration(self, file_writer, .@"enum", enum_decl),
        .union_declaration => |union_decl| try compileCompoundTypeDeclaration(self, file_writer, .@"union", union_decl),
    }
}

fn compileCompoundTypeDeclaration(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    comptime T: enum { @"struct", @"union", @"enum" },
    type_decl: switch (T) {
        .@"struct" => ast.Statement.StructDeclaration,
        .@"union" => ast.Statement.UnionDeclaration,
        .@"enum" => ast.Statement.EnumDeclaration,
    },
) CompilerError!void {
    // register struct in scope
    var compound_type: switch (T) {
        .@"struct" => Type.Struct,
        .@"union" => Type.Union,
        .@"enum" => Type.Enum,
    } = try .init(self.alloc, type_decl.name);

    try self.registerSymbol(type_decl.name, switch (T) {
        .@"struct" => .{ .@"struct" = compound_type },
        .@"union" => .{ .@"union" = compound_type },
        .@"enum" => .{ .@"enum" = compound_type },
    }, .type);

    for (type_decl.members.items) |member| {
        if (compound_type.getProperty(member.name)) |_|
            std.debug.panic(
                "comperr: duplicate member name in compound type declaration: {s}\n",
                .{member.name},
            );

        try compound_type.members.put(member.name, switch (T) {
            // TODO: default values
            .@"struct" => b: {
                const member_type = try self.alloc.create(Type);
                member_type.* = try .fromAst(self, .{ .strong = member.type });
                break :b member_type;
            },
            .@"union" => b: {
                const member_type = try self.alloc.create(Type);
                member_type.* = if (member.type) |t|
                    try .fromAst(self, .{ .strong = t })
                else
                    .void;
                break :b member_type;
            },
            .@"enum" => b: {
                std.debug.print("unimplemented enum explicit values\n", .{});
                break :b null;
            },
        });
    }

    for (type_decl.methods.items) |method| {
        var params: std.ArrayList(*const Type) = try .initCapacity(
            self.alloc,
            method.parameters.items.len,
        );

        for (method.parameters.items) |p| {
            const param_type = try self.alloc.create(Type);
            param_type.* = try .fromAst(self, .{ .strong = p.type });
            params.appendAssumeCapacity(param_type);
        }

        const return_type = try self.alloc.create(Type);
        return_type.* = try .fromAst(self, .{ .strong = method.return_type });
        try compound_type.methods.put(method.name, .{
            .inner_name = try std.fmt.allocPrint(self.alloc, "__zag_{s}_{s}", .{
                type_decl.name,
                method.name,
            }), // TODO mangling
            .params = params,
            .return_type = return_type,
        });
    }

    try self.print(file_writer, "typedef {s} {{\n", .{switch (T) {
        .@"struct" => "struct",
        .@"union" => "union",
        .@"enum" => "enum",
    }});
    self.indent_level += 1;

    var members = compound_type.members.iterator();
    while (members.next()) |member| {
        try self.indent(file_writer);
        switch (T) {
            .@"struct" => {
                try self.compileVariableSignature(
                    file_writer,
                    member.key_ptr.*,
                    member.value_ptr.*.*,
                );
                try self.write(file_writer, ";\n");
            },
            .@"union" => {
                try self.compileVariableSignature(
                    file_writer,
                    member.key_ptr.*,
                    member.value_ptr.*.*,
                );
                try self.write(file_writer, ";\n");
            },
            .@"enum" => {
                try self.print(file_writer, "{s},\n", .{member.key_ptr.*});
                std.debug.print("unimplemented explicit enum member values\n", .{});
            },
        }
    }

    self.indent_level -= 1;
    try self.print(file_writer, "}} {s};\n\n", .{type_decl.name});

    for (type_decl.methods.items) |method| {
        try self.registerSymbol(method.name, try .fromAst(self, .{ .strong = method.getType() }), .symbol);
        try self.pushScope();
        defer self.popScope();

        try self.compileTypeAst(file_writer, method.return_type);
        try self.print(file_writer, " __zag_{s}_{s}(", .{ type_decl.name, method.name }); // TODO: mangling generics
        for (method.parameters.items, 1..) |parameter, i| {
            const parameter_type: Type = try .fromAst(self, .{ .strong = parameter.type });
            try self.registerSymbol(parameter.name, parameter_type, .symbol);
            try self.compileVariableSignature(file_writer, parameter.name, parameter_type);
            if (i < method.parameters.items.len) try self.write(file_writer, ", ");
        }
        try self.write(file_writer, ") ");

        try self.compileBlock(file_writer, method.body);
    }
}

fn compileReturnStatement(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    r: ?ast.Expression,
) CompilerError!void {
    try self.write(file_writer, "return");
    if (r) |*expression| {
        try self.write(file_writer, " ");
        try expressions.compileExpression(self, file_writer, expression);
    }
    try self.write(file_writer, ";\n");
}

fn compileVariableDefinition(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    v: ast.Statement.VariableDefinition,
) CompilerError!void {
    const variable_type: Type = try .fromAst(
        self,
        if (v.type == .inferred)
            .{ .infer = v.assigned_value }
        else
            .{ .strong = v.type },
    );

    if (!v.is_mut) try self.write(file_writer, "const ");

    try self.compileVariableSignature(file_writer, v.variable_name, variable_type);

    try self.write(file_writer, " = ");

    try expressions.compileExpression(self, file_writer, &v.assigned_value);

    try self.write(file_writer, ";\n");

    try self.registerSymbol(v.variable_name, variable_type, .symbol);
}

fn compileConditionalStatement(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    comptime T: enum { @"if", @"while", @"for" },
    statement: switch (T) {
        .@"if" => ast.Statement.If,
        .@"while" => ast.Statement.While,
        .@"for" => ast.Statement.For,
    },
) CompilerError!void {
    try self.print(file_writer, "{s} (", .{switch (T) {
        .@"if" => "if",
        .@"for" => "for",
        .@"while" => "while",
    }});

    switch (T) {
        .@"if", .@"while" => try expressions.compileExpression(self, file_writer, statement.condition),
        .@"for" => switch (statement.iterator.*) {
            .range => |range| {
                try self.compileType(file_writer, try .infer(self, range.start.*));
                try self.print(file_writer, " {s} = ", .{statement.capture});
                try expressions.compileExpression(self, file_writer, range.start);
                try self.print(file_writer, "; {s} < ", .{statement.capture});
                try expressions.compileExpression(self, file_writer, range.end);
                try self.print(file_writer, "; {s}++", .{statement.capture});
            },
            else => |other| switch (try Type.infer(self, other)) {
                .array => std.debug.print("unimplemented array iterator in for loop\n", .{}),
                else => std.debug.print("illegal array iterator type\n", .{}),
            },
        },
    }
    try self.write(file_writer, ") ");

    switch (T) {
        .@"if", .@"while" => if (statement.capture != null)
            std.debug.print("unimplemented conditional statement capture\n", .{}),
        else => {},
    }

    try compileStatement(self, file_writer, statement.body);

    switch (T) {
        .@"if" => if (statement.@"else") |@"else"|
            try compileStatement(self, file_writer, @"else"),
        else => {},
    }
}

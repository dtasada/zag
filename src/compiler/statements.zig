const std = @import("std");

const utils = @import("utils");
const ast = @import("Parser").ast;
const expressions = @import("expressions.zig");

const Type = @import("Type.zig").Type;

const Self = @import("Compiler.zig");
const CompilerError = Self.CompilerError;

pub fn compile(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    statement: *const ast.Statement,
) CompilerError!void {
    switch (statement.*) {
        .function_definition => |fn_def| try functionDefinition(self, file_writer, fn_def),
        .struct_declaration => |struct_decl| try compoundTypeDeclaration(self, file_writer, .@"struct", struct_decl),
        .@"return" => |return_expr| try returnStatement(self, file_writer, return_expr),
        .variable_definition => |var_decl| try variableDefinition(self, file_writer, var_decl),
        .expression => |*expr| {
            try expressions.compile(self, file_writer, expr, .{});
            try self.write(file_writer, ";\n");
        },
        .@"if" => |if_stmt| try conditional(self, file_writer, .@"if", if_stmt),
        .@"while" => |while_stmt| try conditional(self, file_writer, .@"while", while_stmt),
        .@"for" => |for_stmt| try conditional(self, file_writer, .@"for", for_stmt),
        .block => |block| try self.compileBlock(file_writer, block),
        .enum_declaration => |enum_decl| try compoundTypeDeclaration(self, file_writer, .@"enum", enum_decl),
        .union_declaration => |union_decl| try compoundTypeDeclaration(self, file_writer, .@"union", union_decl),
    }
}

fn compoundTypeDeclaration(
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
        if (compound_type.getProperty(member.name)) |_| return utils.printErr(
            error.DuplicateMember,
            "comperr: Duplicate member '{s}' declared in '{s}' at {f}.\n",
            .{ member.name, type_decl.name, try self.parser.getStatementPos(switch (T) {
                .@"struct" => .{ .struct_declaration = type_decl },
                .@"union" => .{ .union_declaration = type_decl },
                .@"enum" => .{ .enum_declaration = type_decl },
            }) },
            .red,
        );

        try compound_type.members.put(member.name, switch (T) {
            // TODO: default values
            .@"struct" => b: {
                const member_type = try self.alloc.create(Type);
                member_type.* = try .fromAst(self, member.type);
                break :b member_type;
            },
            .@"union" => b: {
                const member_type = try self.alloc.create(Type);
                member_type.* = if (member.type) |t|
                    try .fromAst(self, t)
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
            param_type.* = try .fromAst(self, p.type);
            params.appendAssumeCapacity(param_type);
        }

        const return_type = try self.alloc.create(Type);
        return_type.* = try .fromAst(self, method.return_type);
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
                if (member.value_ptr.*) |_|
                    std.debug.print("unimplemented explicit enum member values\n", .{});
            },
        }
    }

    self.indent_level -= 1;
    try self.print(file_writer, "}} {s};\n\n", .{type_decl.name});

    for (type_decl.methods.items) |method| {
        try self.registerSymbol(method.name, try .fromAst(self, method.getType()), .{ .symbol = .{} });
        try self.pushScope();
        defer self.popScope();

        try self.compileType(file_writer, try .fromAst(self, method.return_type));
        try self.print(file_writer, " __zag_{s}_{s}(", .{ type_decl.name, method.name }); // TODO: mangling generics
        for (method.parameters.items, 1..) |parameter, i| {
            const parameter_type: Type = try .fromAst(self, parameter.type);
            try self.registerSymbol(parameter.name, parameter_type, .{ .symbol = .{} });
            try self.compileVariableSignature(file_writer, parameter.name, parameter_type);
            if (i < method.parameters.items.len) try self.write(file_writer, ", ");
        }
        try self.write(file_writer, ") ");

        try self.compileBlock(file_writer, method.body);
    }
}

fn returnStatement(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    r: ?ast.Expression,
) CompilerError!void {
    try self.write(file_writer, "return");
    if (r) |*expression| {
        try self.write(file_writer, " ");
        try expressions.compile(self, file_writer, expression, .{});
    }
    try self.write(file_writer, ";\n");
}

fn variableDefinition(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    v: ast.Statement.VariableDefinition,
) CompilerError!void {
    const variable_type: Type = if (v.type == .inferred)
        try .infer(self, v.assigned_value)
    else
        try .fromAst(self, v.type);

    const received_type: Type = try .infer(self, v.assigned_value);
    if (!variable_type.eq(&received_type)) return utils.printErr(
        error.TypeMismatch,
        "comperr: Type of expression doesn't match explicit type. Expected: '{f}', received '{f}' ({f})\n",
        .{ variable_type, received_type, try self.parser.getExprPos(v.assigned_value) },
        .red,
    );

    if (!v.is_mut) try self.write(file_writer, "const ");

    try self.compileVariableSignature(file_writer, v.variable_name, variable_type);

    try self.write(file_writer, " = ");

    try self.registerSymbol(v.variable_name, variable_type, .{ .symbol = .{ .is_mut = v.is_mut } });

    try expressions.compile(self, file_writer, &v.assigned_value, .{ .is_const = !v.is_mut });

    try self.write(file_writer, ";\n");
}

fn conditional(
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
        .@"if", .@"while" => try expressions.compile(self, file_writer, statement.condition, .{}),
        .@"for" => switch (statement.iterator.*) {
            .range => |range| {
                try self.compileType(file_writer, try .infer(self, range.start.*));
                try self.print(file_writer, " {s} = ", .{statement.capture});
                try expressions.compile(self, file_writer, range.start, .{});
                try self.print(file_writer, "; {s} < ", .{statement.capture});
                try expressions.compile(self, file_writer, range.end, .{});
                try self.print(file_writer, "; {s}++", .{statement.capture});
            },
            else => |other| switch (try Type.infer(self, other)) {
                .array => |array| {
                    try self.compileType(file_writer, .size);
                    try self.print(file_writer, " {s} = 0", .{statement.capture});
                    try self.print(file_writer, "; {s} < {}", .{ statement.capture, array.size orelse
                        @panic("unimplemented: arraylist size") });
                    try self.print(file_writer, "; {s}++", .{statement.capture});
                },
                else => |t| return utils.printErr(
                    error.IllegalExpression,
                    "comperr: Illegal for loop iterator of type '{f}' at {f}.\n",
                    .{ t, try self.parser.getExprPos(statement.iterator.*) },
                    .red,
                ),
            },
        },
    }
    try self.write(file_writer, ") ");

    switch (T) {
        .@"if", .@"while" => if (statement.capture != null)
            std.debug.print("unimplemented conditional statement capture\n", .{}),
        else => {},
    }

    try compile(self, file_writer, statement.body);

    switch (T) {
        .@"if" => if (statement.@"else") |@"else"| {
            try self.indent(file_writer);
            try self.write(file_writer, "else ");
            try compile(self, file_writer, @"else");
        },
        else => {},
    }
}

fn functionDefinition(
    self: *Self,
    file_writer: *std.ArrayList(u8),
    function_def: ast.Statement.FunctionDefinition,
) CompilerError!void {
    try self.registerSymbol(function_def.name, try .fromAst(self, function_def.getType()), .{ .symbol = .{} });

    try self.pushScope();
    defer self.popScope();

    try self.compileType(file_writer, try .fromAst(self, function_def.return_type));
    try self.print(file_writer, " {s}(", .{function_def.name});
    for (function_def.parameters.items, 1..) |parameter, i| {
        try self.compileVariableSignature(file_writer, parameter.name, try .fromAst(self, parameter.type));
        if (i < function_def.parameters.items.len) try self.write(file_writer, ", ");

        try self.registerSymbol(parameter.name, try .fromAst(self, parameter.type), .{ .symbol = .{} });
    }
    try self.write(file_writer, ") ");

    try self.compileBlock(file_writer, function_def.body);
}

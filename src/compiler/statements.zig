const std = @import("std");

const utils = @import("utils");
const ast = @import("Parser").ast;
const expressions = @import("expressions.zig");

const Type = @import("Type.zig").Type;

const Self = @import("Compiler.zig");
const CompilerError = Self.CompilerError;

pub fn compile(
    self: *Self,
    statement: *const ast.Statement,
) CompilerError!void {
    switch (statement.*) {
        .function_definition => |fn_def| try functionDefinition(self, fn_def),
        .struct_declaration => |struct_decl| try compoundTypeDeclaration(self, .@"struct", struct_decl),
        .@"return" => |return_expr| try @"return"(self, return_expr),
        .variable_definition => |var_decl| try variableDefinition(self, var_decl),
        .expression => |*expr| {
            try expressions.compile(self, expr, .{});
            try self.write(";\n");
        },
        .@"if" => |if_stmt| try conditional(self, .@"if", if_stmt),
        .@"while" => |while_stmt| try conditional(self, .@"while", while_stmt),
        .@"for" => |for_stmt| try conditional(self, .@"for", for_stmt),
        .block => |block| try self.compileBlock(block.block, .{}),
        .enum_declaration => |enum_decl| try compoundTypeDeclaration(self, .@"enum", enum_decl),
        .union_declaration => |union_decl| try compoundTypeDeclaration(self, .@"union", union_decl),
    }
}

fn compoundTypeDeclaration(
    self: *Self,
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
            .{ member.name, type_decl.name, type_decl.pos },
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

    try self.print("typedef {s} {{\n", .{switch (T) {
        .@"struct" => "struct",
        .@"union" => "union",
        .@"enum" => "enum",
    }});
    self.indent_level += 1;

    var members = compound_type.members.iterator();
    while (members.next()) |member| {
        try self.indent();
        switch (T) {
            .@"struct" => {
                try self.compileVariableSignature(
                    member.key_ptr.*,
                    member.value_ptr.*.*,
                );
                try self.write(";\n");
            },
            .@"union" => {
                try self.compileVariableSignature(
                    member.key_ptr.*,
                    member.value_ptr.*.*,
                );
                try self.write(";\n");
            },
            .@"enum" => {
                try self.print("{s},\n", .{member.key_ptr.*});
                if (member.value_ptr.*) |_|
                    std.debug.print("unimplemented explicit enum member values\n", .{});
            },
        }
    }

    self.indent_level -= 1;
    try self.print("}} {s};\n\n", .{type_decl.name});

    for (type_decl.methods.items) |method| {
        try self.pushScope();
        defer self.popScope();

        try self.registerSymbol(method.name, try .fromAst(self, method.getType()), .{ .symbol = .{} });

        try self.compileType(try .fromAst(self, method.return_type));
        try self.print(" __zag_{s}_{s}(", .{ type_decl.name, method.name }); // TODO: mangling generics
        for (method.parameters.items, 1..) |parameter, i| {
            const parameter_type: Type = try .fromAst(self, parameter.type);
            try self.registerSymbol(parameter.name, parameter_type, .{ .symbol = .{} });
            try self.compileVariableSignature(parameter.name, parameter_type);
            if (i < method.parameters.items.len) try self.write(", ");
        }
        try self.write(") ");

        try self.compileBlock(method.body, .{});
    }
}

fn @"return"(
    self: *Self,
    r: ast.Statement.Return,
) CompilerError!void {
    const expected_type: Type = b: {
        var scopes = std.mem.reverseIterator(self.scopes.items);
        while (scopes.next()) |scope| {
            var symbols = scope.iterator();
            while (symbols.next()) |symbol|
                switch (symbol.value_ptr.*) {
                    .symbol => |f| switch (f.type) {
                        .function => |function| {
                            const expected_type: Type = function.return_type.*;
                            const received_type: Type = if (r.@"return") |t| try .infer(self, t) else .void;

                            if (!expected_type.eql(received_type) and !received_type.convertsTo(expected_type))
                                return utils.printErr(
                                    error.TypeMismatch,
                                    "comperr: Expected '{f}', received '{f}' ({f}).\n",
                                    .{ expected_type, received_type, r.pos },
                                    .red,
                                );

                            break :b expected_type;
                        },
                        else => continue,
                    },
                    .type => continue,
                };
        }

        return utils.printErr(
            error.IllegalStatement,
            "comperr: return statement not in a function ({f}).\n",
            .{r.pos},
            .red,
        );
    };

    try self.write("return");
    if (r.@"return") |*expression| {
        try self.write(" ");
        try expressions.compile(self, expression, .{ .expected_type = expected_type });
    }
    try self.write(";\n");
}

fn variableDefinition(
    self: *Self,
    v: ast.Statement.VariableDefinition,
) CompilerError!void {
    const received_type: Type = try .infer(self, v.assigned_value);
    const expected_type: ?Type = if (v.type == .inferred) null else try .fromAst(self, v.type);

    if (expected_type != null and
        !expected_type.?.eql(received_type) and
        !received_type.convertsTo(expected_type.?))
        return utils.printErr(
            error.TypeMismatch,
            "comperr: Type of expression doesn't match explicit type. Expected: '{f}', received '{f}' ({f}).\n",
            .{ expected_type.?, received_type, v.assigned_value.getPosition() },
            .red,
        );

    if (!v.is_mut and received_type != .function) try self.write("const ");

    try self.compileVariableSignature(v.variable_name, expected_type orelse received_type);

    if (v.assigned_value != .ident and std.mem.eql(u8, v.assigned_value.ident.ident, "undefined")) {
        try self.write(" = ");

        try expressions.compile(self, &v.assigned_value, .{
            .is_const = !v.is_mut,
            .expected_type = expected_type,
        });
    }

    try self.registerSymbol(
        v.variable_name,
        expected_type orelse received_type,
        .{ .symbol = .{ .is_mut = v.is_mut } },
    );

    try self.write(";\n");
}

fn conditional(
    self: *Self,
    comptime T: enum { @"if", @"while", @"for" },
    statement: switch (T) {
        .@"if" => ast.Statement.If,
        .@"while" => ast.Statement.While,
        .@"for" => ast.Statement.For,
    },
) CompilerError!void {
    try self.print("{s} (", .{switch (T) {
        .@"if" => "if",
        .@"for" => "for",
        .@"while" => "while",
    }});

    if (statement.capture) |capture|
        if (T != .@"for" and try Type.infer(self, statement.condition.*) != .optional)
            return utils.printErr(
                error.IllegalExpression,
                "comperr: {s} statement contains capture '{s}' but condition is not an optional ({f}).\n",
                .{ @tagName(T), capture, statement.pos },
                .red,
            );

    var capture_ident: []const u8 = undefined;
    switch (T) {
        .@"if", .@"while" => switch (try Type.infer(self, statement.condition.*)) {
            .bool, .optional => try expressions.compile(self, statement.condition, .{}),
            else => |t| return utils.printErr(
                error.IllegalExpression,
                "comperr: Illegal expression: {s} statement condition must be a boolean or an optional, received {f} ({f}).\n",
                .{ @tagName(T), t, statement.pos },
                .red,
            ),
        },
        .@"for" => {
            switch (statement.iterator.*) {
                .range => |range| {
                    capture_ident = statement.capture orelse
                        try std.fmt.allocPrint(self.alloc, "_{}", .{utils.randInt(u64)});

                    try self.compileType(try .infer(self, range.start.*));
                    try self.print(" {s} = ", .{capture_ident});
                    try expressions.compile(self, range.start, .{});
                    try self.print("; {s} {s} ", .{ capture_ident, if (range.inclusive) "<=" else "<" });
                    try expressions.compile(self, range.end, .{});
                    try self.print("; {s}++", .{capture_ident});
                },
                else => |other| switch (try Type.infer(self, other)) {
                    .array => |array| {
                        capture_ident = try std.fmt.allocPrint(self.alloc, "_{}", .{utils.randInt(u64)});

                        try self.compileType(.usize);
                        try self.print(" {s} = 0", .{capture_ident});
                        try self.print("; {s} < ", .{capture_ident});
                        if (array.size) |size| {
                            try self.print("{}", .{size});
                        } else {
                            try self.write("(");
                            try expressions.compile(self, statement.iterator, .{});
                            try self.write(").len");
                        }
                        try self.print("; {s}++", .{capture_ident});
                    },
                    else => |t| return utils.printErr(
                        error.IllegalExpression,
                        "comperr: Illegal for loop iterator of type '{f}' at {f}.\n",
                        .{ t, statement.iterator.getPosition() },
                        .red,
                    ),
                },
            }
        },
    }
    try self.write(") ");

    try self.compileBlock(
        switch (statement.body.*) {
            .block => |block| block.block,
            else => b: {
                var slice = [1]ast.Statement{statement.body.*};
                break :b ast.Block.fromOwnedSlice(&slice);
            },
        },
        switch (T) {
            .@"for" => .{
                .iterator = if (statement.capture) |c| .{
                    .iter_expr = statement.iterator,
                    .capture_name = c,
                    .index = capture_ident,
                } else null,
            },
            else => .{
                .capture = if (statement.capture) |c| .{
                    .condition = statement.condition,
                    .name = c,
                } else null,
            },
        },
    );

    switch (T) {
        .@"if" => if (statement.@"else") |@"else"| {
            try self.indent();
            try self.write("else ");
            try compile(self, @"else");
        },
        else => {},
    }
}

fn functionDefinition(
    self: *Self,
    function_def: ast.Statement.FunctionDefinition,
) CompilerError!void {
    try self.registerSymbol(function_def.name, try .fromAst(self, function_def.getType()), .{ .symbol = .{} });

    try self.pushScope();
    defer self.popScope();

    try self.compileType(try .fromAst(self, function_def.return_type));
    try self.print(" {s}(", .{function_def.name});
    for (function_def.parameters.items, 1..) |parameter, i| {
        try self.compileVariableSignature(parameter.name, try .fromAst(self, parameter.type));
        if (i < function_def.parameters.items.len) try self.write(", ");

        try self.registerSymbol(parameter.name, try .fromAst(self, parameter.type), .{ .symbol = .{} });
    }
    try self.write(") ");

    try self.compileBlock(function_def.body, .{});
}

const std = @import("std");

const utils = @import("utils");
const ast = @import("Parser").ast;
const expressions = @import("expressions.zig");
const errors = @import("errors.zig");

const Type = @import("Type.zig").Type;
const Module = @import("Module.zig");

const Self = @import("Compiler.zig");
const CompilerError = errors.CompilerError;

pub fn compile(self: *Self, statement: *const ast.Statement) CompilerError!void {
    switch (statement.*) {
        .function_definition => |*fn_def| try functionDefinition(self, false, fn_def),
        .import => |import_statement| try import(self, import_statement),
        .binding_function_declaration => |*bind| try functionDefinition(self, true, bind),
        .struct_declaration => |*struct_decl| try compoundTypeDeclaration(self, .@"struct", struct_decl),
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
        .enum_declaration => |*enum_decl| try compoundTypeDeclaration(self, .@"enum", enum_decl),
        .union_declaration => |*union_decl| try compoundTypeDeclaration(self, .@"union", union_decl),
        .@"break", .@"continue" => try self.print("{s};\n", .{@tagName(statement.*)}),
    }
}

fn compoundTypeDeclaration(
    self: *Self,
    comptime T: enum { @"struct", @"union", @"enum" },
    type_decl: *const switch (T) {
        .@"struct" => ast.Statement.StructDeclaration,
        .@"union" => ast.Statement.UnionDeclaration,
        .@"enum" => ast.Statement.EnumDeclaration,
    },
) CompilerError!void {
    const compound_type = try Type.fromCompoundTypeDeclaration(self, switch (T) {
        .@"struct" => .@"struct",
        .@"union" => .@"union",
        .@"enum" => .@"enum",
    }, type_decl);

    switch (T) {
        .@"struct" => if (type_decl.generic_types) |g| if (g.items.len > 0) return,
        .@"union" => if (type_decl.generic_types) |g| if (g.items.len > 0) return,
        else => {},
    }

    if (type_decl.is_pub and self.module_header != null)
        try self.switchWriter(.module_header);

    try self.print("typedef {s} {{\n", .{switch (T) {
        .@"struct" => "struct",
        .@"union" => "struct",
        .@"enum" => "enum",
    }});

    self.indent_level += 1;

    var members = compound_type.members.iterator();
    if (T == .@"union") {
        try self.indent();
        try self.print("{f} tag;\n", .{compound_type.tag_type.?});
        try self.indent();
        try self.write("union {\n");
        self.indent_level += 1;
    }

    while (members.next()) |member| {
        try self.indent();
        switch (T) {
            .@"struct" => {
                try self.compileVariableSignature(
                    member.key_ptr.*,
                    member.value_ptr.*.*,
                    .{ .binding_mut = true }, // struct members shouldn't be const.
                );
                try self.write(";\n");
            },
            .@"union" => {
                try self.compileVariableSignature(
                    member.key_ptr.*,
                    member.value_ptr.*.*,
                    .{ .binding_mut = true }, // struct members shouldn't be const.
                );
                try self.write(";\n");
            },
            .@"enum" => try self.print("__zag_{s}_{s} = {},\n", .{
                compound_type.name,
                member.key_ptr.*,
                member.value_ptr.*,
            }),
        }
    }
    if (T == .@"union") {
        self.indent_level -= 1;
        try self.indent();
        try self.write("} payload;\n");
    }

    self.indent_level -= 1;
    try self.print("}} {s};\n\n", .{compound_type.name});

    try self.switchWriter(.output);

    for (type_decl.methods.items) |method| {
        try self.pushScope();
        defer self.popScope();

        // Register struct's generic parameters if this is an instantiation
        // Check if we have generic params in outer scope
        const outer_scope_idx = self.scopes.items.len - 2; // -1 is current, -2 is outer
        if (outer_scope_idx >= 0) {
            var outer_scope = &self.scopes.items[outer_scope_idx];
            switch (T) {
                .@"struct", .@"union" => {
                    if (type_decl.generic_types) |generic_types| {
                        for (generic_types.items) |g| {
                            // Copy from outer scope if it exists there
                            if (outer_scope.get(g.name)) |outer_item| {
                                try self.scopes.items[self.scopes.items.len - 1].put(g.name, outer_item);
                            }
                        }
                    }
                },
                else => {},
            }
        }

        const previous_return_type = self.current_return_type;
        defer self.current_return_type = previous_return_type;
        self.current_return_type = try Type.fromAst(self, method.return_type);

        try self.registerSymbol(method.name, .{ .symbol = .{ .type = try .fromAst(self, method.getType()) } });

        if (type_decl.is_pub and self.module_header != null) {
            try self.switchWriter(.module_header);

            try self.compileType(try .fromAst(self, method.return_type), .{ .binding_mut = true });
            try self.print(" __zag_{s}_{s}(", .{ compound_type.name, method.name });
            for (method.parameters.items, 1..) |parameter_type, i| {
                try self.compileVariableSignature(parameter_type.name, try .fromAst(self, parameter_type.type), .{});
                if (i < method.parameters.items.len) try self.write(", ");
            }
            try self.write(");\n");

            try self.switchWriter(.output);
        }

        try self.compileType(try .fromAst(self, method.return_type), .{ .binding_mut = true });
        try self.print(" __zag_{s}_{s}(", .{ compound_type.name, method.name }); // TODO: mangling generics
        for (method.parameters.items, 1..) |parameter, i| {
            const param_type: Type = try .fromAst(self, parameter.type);
            try self.registerSymbol(parameter.name, .{ .symbol = .{ .type = param_type } });
            try self.compileVariableSignature(parameter.name, param_type, .{});
            if (i < method.parameters.items.len) try self.write(", ");
        }
        try self.write(") ");

        try self.compileBlock(method.body, .{});
    }
}

fn @"return"(self: *Self, return_expr: ast.Statement.Return) CompilerError!void {
    const expected_type = self.current_return_type orelse return utils.printErr(
        error.IllegalStatement,
        "comperr: Return statement outside of function ({f}).\n",
        .{return_expr.pos},
        .red,
    );

    if (return_expr.@"return") |expr| {
        try self.write("return ");
        try expressions.compile(self, &expr, .{ .expected_type = expected_type });
        try self.write(";\n");
    } else {
        if (expected_type != .void) return utils.printErr(
            error.MissingReturnType,
            "comperr: Expected return value of type '{f}' ({f}).\n",
            .{ expected_type, return_expr.pos },
            .red,
        );
        try self.write("return;\n");
    }
}

fn variableDefinition(self: *Self, v: ast.Statement.VariableDefinition) CompilerError!void {
    const received_type: Type = try .infer(self, v.assigned_value);
    const expected_type: ?Type = if (v.type == .inferred) null else try .fromAst(self, v.type);

    if (self.getScopeItem(v.variable_name)) |_| return utils.printErr(
        error.IllegalStatement,
        "comperr: Symbol shadowing is not allowed: attempt to redeclare '{s}' ({f}).\n",
        .{ v.variable_name, v.pos },
        .red,
    ) else |_| {}

    if (expected_type) |et| if (!received_type.check(et))
        return errors.typeMismatch(
            expected_type.?,
            received_type,
            v.assigned_value.getPosition(),
        );

    try self.compileVariableSignature(v.variable_name, expected_type orelse received_type, .{ .binding_mut = v.is_mut });

    if (v.assigned_value != .ident or !std.mem.eql(u8, v.assigned_value.ident.ident, "undefined")) {
        try self.write(" = ");

        try expressions.compile(self, &v.assigned_value, .{
            .binding_mut = v.is_mut,
            .expected_type = expected_type,
        });
    }

    try self.registerSymbol(v.variable_name, .{
        .symbol = .{
            .is_mut = v.is_mut,
            .type = expected_type orelse received_type,
        },
    });

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
        if (T != .@"for" and try Type.infer(self, statement.condition) != .optional)
            return utils.printErr(
                error.IllegalExpression,
                "comperr: {s} statement contains capture '{s}' but condition is not an optional ({f}).\n",
                .{ @tagName(T), capture, statement.pos },
                .red,
            );

    var capture_ident: []const u8 = undefined;
    switch (T) {
        .@"if", .@"while" => switch (try Type.infer(self, statement.condition)) {
            .bool, .optional => try expressions.compile(self, &statement.condition, .{}),
            else => |t| return utils.printErr(
                error.IllegalExpression,
                "comperr: Illegal expression: {s} statement condition must be a boolean or an optional, received {f} ({f}).\n",
                .{ @tagName(T), t, statement.pos },
                .red,
            ),
        },
        .@"for" => switch (statement.iterator) {
            .range => |range| {
                capture_ident = statement.capture orelse
                    try std.fmt.allocPrint(self.alloc, "_{}", .{utils.randInt(u64)});

                try self.compileVariableSignature(capture_ident, try .infer(self, range.start.*), .{ .binding_mut = true });
                try self.write(" = ");
                try expressions.compile(self, range.start, .{});
                try self.write(";");

                try self.write("(");
                try expressions.compile(self, range.end, .{});
                try self.write(">");
                try expressions.compile(self, range.start, .{});
                try self.write(") ? ");
                try self.print("{s} {s} ", .{ capture_ident, if (range.inclusive) "<=" else "<" });
                try expressions.compile(self, range.end, .{});

                try self.write(" : ");
                try self.print("{s} {s} ", .{ capture_ident, if (range.inclusive) ">=" else ">" });
                try expressions.compile(self, range.end, .{});
                try self.write("; ");

                try self.write("(");
                try expressions.compile(self, range.end, .{});
                try self.write(">");
                try expressions.compile(self, range.start, .{});
                try self.write(") ? ");
                try self.print("{s}++ ", .{capture_ident});
                try self.write(" : ");
                try self.print("{s}-- ", .{capture_ident});

                if (statement.capture) |capture|
                    try self.registerSymbol(capture, .{ .symbol = .{ .type = .usize } });
            },
            else => |other| {
                capture_ident = try std.fmt.allocPrint(self.alloc, "_{}", .{utils.randInt(u64)});

                try self.compileType(.usize, .{ .binding_mut = true });
                try self.print(" {s} = 0", .{capture_ident});
                try self.print("; {s} < (", .{capture_ident});

                switch (try Type.infer(self, other)) {
                    .array => |array| try self.print("{}); {s}++", .{ array.size, capture_ident }),
                    .slice => {
                        try expressions.compile(self, &other, .{});
                        try self.print(").len; {s}++", .{capture_ident});
                    },
                    else => |t| return utils.printErr(
                        error.IllegalExpression,
                        "comperr: Illegal for loop iterator of type '{f}' at {f}.\n",
                        .{ t, statement.iterator.getPosition() },
                        .red,
                    ),
                }
            },
        },
    }
    try self.write(") ");

    try self.compileBlock(
        switch (statement.body.*) {
            .block => |block| block.block,
            else => b: {
                var slice = [_]ast.Statement{statement.body.*};
                break :b ast.Block.fromOwnedSlice(&slice);
            },
        },
        switch (T) {
            .@"for" => .{
                .iterator = if (statement.capture != null and statement.iterator != .range) .{
                    .iter_expr = &statement.iterator,
                    .capture_name = statement.capture.?,
                    .index = capture_ident,
                } else null,
            },
            else => .{
                .capture = if (statement.capture) |c| .{
                    .condition = &statement.condition,
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
    comptime binding_function: bool,
    function_def: *const if (binding_function)
        ast.Statement.BindingFunctionDeclaration
    else
        ast.Statement.FunctionDefinition,
) CompilerError!void {
    _ = self.getSymbolType(function_def.name) catch
        try self.registerSymbol(function_def.name, .{ .symbol = .{
            .type = b: {
                var type_obj = try Type.fromAst(self, function_def.getType());
                if (!binding_function)
                    type_obj.function.definition = function_def;

                break :b type_obj;
            },
        } });

    if (!binding_function and function_def.generic_parameters.items.len > 0) return;

    try self.pushScope();
    defer self.popScope();

    const previous_return_type = self.current_return_type;
    defer self.current_return_type = previous_return_type;
    self.current_return_type = try Type.fromAst(self, function_def.return_type);

    if (binding_function or function_def.is_pub and self.module_header != null) {
        try self.switchWriter(.module_header);
        defer self.switchWriterBack();

        // we'll set the type of the function return type to be mutable because cc warns when a
        // function's return type is `const` qualified.
        try self.compileType(try .fromAst(self, function_def.return_type), .{ .binding_mut = true });
        try self.print(" {s}(", .{function_def.name});
        for (function_def.parameters.items, 1..) |parameter, i| {
            try self.compileVariableSignature(parameter.name, try .fromAst(self, parameter.type), .{});
            if (i < function_def.parameters.items.len) try self.write(", ");
        }
        try self.write(");\n");
    }

    if (!binding_function) {
        // we'll set the type of the function return type to be mutable because cc warns when a
        // function's return type is `const` qualified.
        try self.compileType(try .fromAst(self, function_def.return_type), .{ .binding_mut = true });
        try self.print(" {s}(", .{function_def.name});
        for (function_def.parameters.items, 1..) |parameter, i| {
            try self.compileVariableSignature(parameter.name, try .fromAst(self, parameter.type), .{});
            if (i < function_def.parameters.items.len) try self.write(", ");

            try self.registerSymbol(parameter.name, .{
                .symbol = .{ .type = try .fromAst(self, parameter.type) },
            });
        }
        try self.write(") ");

        try self.compileBlock(function_def.body, .{});

        // if (!guarantee_return) return utils.printErr(
        //     error.MissingReturnStatement,
        //     "comperr: Function '{s}' must return '{f}' on all code paths ({f}).\n",
        //     .{ function_def.name, try Type.fromAst(self, function_def.return_type), function_def.pos },
        //     .red,
        // );
    }
}

fn import(self: *Self, statement: ast.Statement.Import) CompilerError!void {
    const module = try self.processImport(&statement);

    const name = statement.alias orelse statement.module_name.getLast();
    try self.registerSymbol(
        name,
        .{ .module = module },
    );

    // Emit #include "path/to/header.h"
    try self.write("#include \"");
    for (statement.module_name.items, 0..) |part, i| {
        try self.write(part);
        if (i < statement.module_name.items.len - 1) try self.write("/");
    }
    try self.write(".zag.h\"\n");
}

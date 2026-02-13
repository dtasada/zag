const std = @import("std");

const utils = @import("utils");
const ast = @import("Parser").ast;
const expressions = @import("expressions.zig");
const errors = @import("errors.zig");
const analysis = @import("analysis.zig");

const Type = @import("Type.zig").Type;
const Module = @import("Module.zig");

const Self = @import("Compiler.zig");
const CompilerError = errors.CompilerError;

pub fn compile(self: *Self, statement: *const ast.Statement) CompilerError!void {
    self.currentSection().current_statement = self.currentWriter().items.len;

    switch (statement.*) {
        .function_definition => |*fn_def| try functionDefinition(self, false, fn_def),
        .import => |import_statement| try import(self, import_statement),
        .binding_function_declaration => |*bind| try functionDefinition(self, true, bind),
        .struct_declaration => |*struct_decl| try compoundTypeDeclaration(self, .@"struct", struct_decl),
        .@"return" => |return_expr| try @"return"(self, return_expr),
        .variable_definition => |var_decl| try variableDefinition(self, var_decl, .{}),
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
        .block_eval => |block_eval| try @"return"(self, .{
            .pos = block_eval.getPosition(),
            .@"return" = block_eval,
        }),
        .binding_type_declaration => |btd| {
            const t: Type = switch (btd.type) {
                .@"struct" => .{ .@"struct" = try Type.Struct.init(self, btd.name, btd.name, null) },
                .@"union" => .{ .@"union" = try Type.Union.init(self, btd.name, btd.name, null) },
                .@"enum" => .{ .@"enum" = try Type.Enum.init(self, btd.name, btd.name, null) },
            };
            try self.registerSymbol(btd.name, .{ .type = t }, .{});

            const saved_section = self.current_section;
            self.switchSection(.header_forward_decls);
            defer self.switchSection(saved_section);

            try self.print("typedef {s} {s} {s};\n", .{ @tagName(btd.type), btd.name, btd.name });
        },
        .@"defer" => |d| {
            var pending_defers = self.scopes.getLast().pending_defers;
            try pending_defers.append(self.alloc, d.stmt);
        },
    }
}

fn compoundTypeDeclaration(
    self: *Self,
    comptime T: utils.CompoundTypeTag,
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
        inline .@"struct", .@"union" => if (type_decl.generic_types.items.len > 0) return,
        else => {},
    }

    const saved_section = self.current_section;

    self.switchSection(if (type_decl.is_pub) .header_type_defs else .source_type_impls);
    defer self.switchSection(saved_section);

    const inner_name = try self.getInnerName(type_decl.name);
    const structure_type = switch (T) {
        .@"struct", .@"union" => "struct",
        .@"enum" => "enum",
    };

    self.switchSection(.header_forward_decls);
    try self.print("typedef {s} {s} {s};\n", .{ structure_type, inner_name, inner_name });
    self.switchSection(if (type_decl.is_pub) .header_type_defs else .source_type_impls);

    for (type_decl.subtypes.items) |subtype| switch (subtype) {
        inline else => |st, tag| try compoundTypeDeclaration(self, tag, &st),
    };

    // Emit the tag type enum for unions
    if (T == .@"union") {
        const tag_enum = compound_type.tag_type.?.@"enum";

        try self.print("typedef enum {s} {{\n", .{tag_enum.inner_name});
        self.indent_level += 1;

        var members = tag_enum.members.iterator();
        while (members.next()) |member| {
            try self.indent();
            try self.print("__zag_{s}_{s} = {},\n", .{
                tag_enum.inner_name,
                member.key_ptr.*,
                member.value_ptr.*,
            });
        }

        self.indent_level -= 1;
        try self.print("}} {s};\n\n", .{tag_enum.inner_name});
    }

    try self.print("typedef {s} {s} {{\n", .{ structure_type, inner_name });

    self.indent_level += 1;

    var members = compound_type.members.iterator();
    if (T == .@"union") {
        try self.indent();
        try self.print("{s} tag;\n", .{compound_type.tag_type.?.@"enum".inner_name});
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
                var t = member.value_ptr.*.*;
                if (t == .void) t = .u8;
                try self.compileVariableSignature(member.key_ptr.*, t, .{ .binding_mut = true });
                try self.write(";\n");
            },
            .@"enum" => try self.print("{s}_{s} = {d},\n", .{
                inner_name,
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
    try self.print("}} {s};\n\n", .{inner_name});

    for (type_decl.variables.items) |variable|
        try variableDefinition(self, variable, .{
            .inner_name = compound_type
                .variables
                .get(variable.variable_name).?
                .inner_name,
        });

    self.switchSection(.source_function_impls);
    for (type_decl.methods.items) |method| {
        try self.pushScope();
        defer self.popScope();

        // Register struct's generic parameters if this is an instantiation
        // Check if we have generic params in outer scope
        const outer_scope_idx = self.scopes.items.len - 2; // -1 is current, -2 is outer
        if (outer_scope_idx >= 0) switch (T) {
            .@"struct", .@"union" => for (type_decl.generic_types.items) |g| {
                // Copy from outer scope if it exists there
                if (self.scopes.items[outer_scope_idx].items.get(g.name)) |outer_item|
                    try self.scopes.getLast().items.put(g.name, outer_item);
            },
            else => {},
        };

        const previous_return_type = self.current_return_type;
        defer self.current_return_type = previous_return_type;
        self.current_return_type = try Type.fromAst(self, method.return_type);

        try self.registerSymbol(
            method.name,
            .{ .symbol = .{ .type = try .fromAst(self, method.getType()) } },
            .{ .inner_name = try self.mangle(method.name) },
        );

        if (type_decl.is_pub) {
            self.switchSection(.header_function_decls);

            try self.compileType(try .fromAst(self, method.return_type), .{ .binding_mut = true });
            try self.print(" __zag_{s}_{s}(", .{ compound_type.name, method.name });
            for (method.parameters.items, 0..) |parameter_type, i| {
                try self.compileVariableSignature(parameter_type.name, try .fromAst(self, parameter_type.type), .{});
                if (i < method.parameters.items.len - 1) try self.write(", ");
            }
            try self.write(");\n");

            self.switchSection(.source_function_impls);
        }

        try self.compileType(try .fromAst(self, method.return_type), .{ .binding_mut = true });
        try self.print(" __zag_{s}_{s}(", .{ compound_type.name, method.name });
        for (method.parameters.items, 0..) |parameter, i| {
            const param_type: Type = try .fromAst(self, parameter.type);
            try self.registerSymbol(parameter.name, .{ .symbol = .{ .type = param_type } }, .{});
            try self.compileVariableSignature(parameter.name, param_type, .{});
            if (i < method.parameters.items.len - 1) try self.write(", ");
        }
        try self.write(") ");

        const returns_on_all_paths = analysis.blockReturns(method.body);

        const inject_return: ?Type = switch (self.current_return_type.?) {
            .void => null,
            .error_union => |error_union| if (error_union.success.* == .void)
                // return dummy 0 when return type is !void, since unions in C can't have void members
                self.current_return_type.?
            else if (returns_on_all_paths) null else return errors.functionMustReturn(
                method.name,
                self.current_return_type.?,
                method.pos,
            ),
            else => if (returns_on_all_paths) null else return errors.functionMustReturn(
                method.name,
                self.current_return_type.?,
                method.pos,
            ),
        };

        try self.compileBlock(method.body, .{ .return_implicit_success = inject_return });
    }
}

pub fn @"return"(self: *Self, return_expr: ast.Statement.Return) CompilerError!void {
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

fn variableDefinition(
    self: *Self,
    v: ast.Statement.VariableDefinition,
    opts: struct { inner_name: ?[]const u8 = null },
) CompilerError!void {
    const received_type: Type = try .infer(self, v.assigned_value);
    const expected_type: ?Type = if (v.type == .inferred) null else try .fromAst(self, v.type);
    const final_type = expected_type orelse received_type;

    if (self.getScopeItem(v.variable_name)) |item| {
        if (item == .symbol and item.symbol.is_defined)
            return errors.symbolShadowing(v.variable_name, v.pos);
    } else |_| {}

    if (expected_type) |et| if (!received_type.check(et))
        return errors.typeMismatch(et, received_type, v.assigned_value.getPosition());

    const mangled_name = opts.inner_name orelse try self.mangle(v.variable_name);

    if (final_type == .type) {
        try self.write("typedef ");
        try expressions.compile(self, &v.assigned_value, .{ .binding_mut = true });
        try self.print(" {s}", .{mangled_name});
        try self.write(";\n");
        try self.registerSymbol(v.variable_name, .{ .type = final_type }, .{});
    } else {
        try self.registerSymbol(v.variable_name, .{
            .symbol = .{
                .is_mut = v.binding == .is_mut,
                .type = final_type,
            },
        }, .{ .inner_name = mangled_name });

        if (v.is_pub) {
            const saved_section = self.current_section;
            self.switchSection(.header_forward_decls);
            defer self.switchSection(saved_section);

            try self.write("extern ");
            // For extern, it's a declaration. binding_mut=false gives 'const' for let/const.
            try self.compileVariableSignature(mangled_name, final_type, .{ .binding_mut = v.binding == .is_mut });
            try self.write(";\n");
        }

        try self.compileVariableSignature(mangled_name, final_type, .{
            .binding_mut = v.binding == .is_mut,
            .is_const = v.binding == .is_const and !v.is_pub,
        });

        if (v.assigned_value != .ident or !std.mem.eql(u8, v.assigned_value.ident.ident, "undefined")) {
            try self.write(" = ");

            try expressions.compile(self, &v.assigned_value, .{
                .binding_mut = v.binding == .is_mut,
                .expected_type = expected_type,
                .is_variable_declaration = true,
            });
        }
        try self.write(";\n");
    }
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
                const end = range.end orelse return utils.printErr(
                    error.IllegalExpression,
                    "comperr: For statement iterator range expression must contain an end ({f}).\n",
                    .{range.pos},
                    .red,
                );

                try expressions.compile(self, end, .{});
                try self.write(">");
                try expressions.compile(self, range.start, .{});
                try self.write(") ? ");
                try self.print("{s} {s} ", .{ capture_ident, if (range.inclusive) "<=" else "<" });
                try expressions.compile(self, end, .{});

                try self.write(" : ");
                try self.print("{s} {s} ", .{ capture_ident, if (range.inclusive) ">=" else ">" });
                try expressions.compile(self, end, .{});
                try self.write("; ");

                try self.write("(");
                try expressions.compile(self, end, .{});
                try self.write(">");
                try expressions.compile(self, range.start, .{});
                try self.write(") ? ");
                try self.print("{s}++ ", .{capture_ident});
                try self.write(" : ");
                try self.print("{s}-- ", .{capture_ident});

                if (statement.capture) |capture|
                    try self.registerSymbol(capture, .{ .symbol = .{ .type = .usize } }, .{});
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
                var block: ast.Block = .empty;
                try block.append(self.alloc, statement.body.*);
                break :b block;
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
    const inner_name = if (binding_function) function_def.name else try self.mangle(function_def.name);

    if (self.getSymbolDefined(function_def.name)) |sd| {
        if (sd) return errors.symbolShadowing(function_def.name, function_def.pos);
    } else |err| switch (err) {
        error.SymbolNotVariable => return err,
        else => {},
    }

    try self.registerSymbol(function_def.name, .{ .symbol = .{
        .type = b: {
            var type_obj = try Type.fromAst(self, function_def.getType());
            if (!binding_function)
                type_obj.function.definition = function_def;

            break :b type_obj;
        },
    } }, .{ .inner_name = inner_name });

    if (!binding_function and function_def.generic_parameters.items.len > 0) return;

    try self.pushScope();
    defer self.popScope();

    const previous_return_type = self.current_return_type;
    defer self.current_return_type = previous_return_type;
    self.current_return_type = try .fromAst(self, function_def.return_type);

    if (binding_function or function_def.is_pub) {
        const saved_section = self.current_section;
        self.switchSection(.header_forward_decls);
        defer self.switchSection(saved_section);

        // we'll set the type of the function return type to be mutable because cc warns when a
        // function's return type is `const` qualified.
        try self.compileType(self.current_return_type.?, .{ .binding_mut = true });
        try self.print(" {s}(", .{inner_name});
        for (function_def.parameters.items, 0..) |parameter, i| {
            try self.compileVariableSignature(parameter.name, try .fromAst(self, parameter.type), .{ .binding_mut = true });
            if (i < function_def.parameters.items.len - 1) try self.write(", ");
        }
        try self.write(");\n");
    }

    if (!binding_function) {
        // we'll set the type of the function return type to be mutable because cc warns when a
        // function's return type is `const` qualified.
        try self.compileType(try .fromAst(self, function_def.return_type), .{ .binding_mut = true });
        try self.print(" {s}(", .{inner_name});
        for (function_def.parameters.items, 0..) |parameter, i| {
            try self.compileVariableSignature(
                parameter.name,
                try .fromAst(self, parameter.type),
                .{ .binding_mut = parameter.is_mut },
            );
            if (i < function_def.parameters.items.len - 1) try self.write(", ");

            try self.registerSymbol(parameter.name, .{
                .symbol = .{
                    .type = try .fromAst(self, parameter.type),
                    .is_mut = parameter.is_mut,
                },
            }, .{});
        }
        try self.write(") ");

        const returns_on_all_paths = analysis.blockReturns(function_def.body);

        const inject_return: ?Type = switch (self.current_return_type.?) {
            .void => null,
            .error_union => |error_union| if (error_union.success.* == .void)
                // return dummy 0 when return type is !void, since unions in C can't have void members
                self.current_return_type.?
            else if (returns_on_all_paths) null else return errors.functionMustReturn(
                function_def.name,
                self.current_return_type.?,
                function_def.pos,
            ),
            else => if (returns_on_all_paths) null else return errors.functionMustReturn(
                function_def.name,
                self.current_return_type.?,
                function_def.pos,
            ),
        };

        try self.compileBlock(function_def.body, .{ .return_implicit_success = inject_return });
    }
}

fn import(self: *Self, statement: ast.Statement.Import) CompilerError!void {
    const module = try self.processImport(&statement);

    const name = statement.alias orelse statement.module_name.getLast();
    try self.registerSymbol(name, .{ .module = module }, .{});

    // Emit #include "path/to/header.h"
    try self.write("#include \"");
    for (statement.module_name.items, 0..) |part, i| {
        try self.write(part);
        if (i < statement.module_name.items.len - 1) try self.write("/");
    }
    try self.write(".zag.h\"\n");
}

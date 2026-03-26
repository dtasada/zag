const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const expressions = @import("expressions.zig");

const errors = @import("errors.zig");
const Error = errors.Error;
const Compiler = @import("compiler.zig").Compiler;
const Type = @import("type.zig").Type;

pub fn compile(alloc: std.mem.Allocator, statement: ast.Statement, c: *Compiler) Error![]const u8 {
    return switch (statement) {
        inline .@"break", .@"continue" => |_, tag| try alloc.dupe(u8, @tagName(tag)),
        .@"for" => |cond| try conditional(alloc, .@"for", cond, c),
        .@"while" => |cond| try conditional(alloc, .@"while", cond, c),
        .@"if" => |cond| try conditional(alloc, .@"if", cond, c),
        .@"return" => |ret| try expressions.@"return"(alloc, ret, c),
        .expression => |*expr| {
            const expr_comp = try expressions.compile(alloc, expr, c, .{});
            defer alloc.free(expr_comp);
            return try std.fmt.allocPrint(alloc, "{s};", .{expr_comp});
        },
        .block_eval => |*eval| try expressions.compile(alloc, eval, c, .{}),
        .block => |b| block(alloc, b.payload, c),
        .variable_definition => |vd| try variableDefinition(alloc, vd, c),
        else => unreachable,
    };
}

fn conditional(
    alloc: std.mem.Allocator,
    comptime tag: utils.ConditionalTag,
    statement: switch (tag) {
        .@"for" => ast.Statement.For,
        .@"while" => ast.Statement.While,
        .@"if" => ast.Statement.If,
    },
    c: *Compiler,
) ![]const u8 {
    _ = alloc;
    _ = statement;
    _ = c;
    unreachable;
}

pub fn compileTopLevel(alloc: std.mem.Allocator, statement: ast.TopLevelStatement, c: *Compiler) !void {
    switch (statement) {
        .import => |s| try import(alloc, c, s),
        .binding_function_declaration => |bfd| {
            const function_type_ast = try bfd.getType(alloc);
            defer function_type_ast.deinit(alloc);

            const function_type: Type = try .fromAst(alloc, &function_type_ast, c);
            errdefer function_type.deinit(alloc);

            try c.module.register(alloc, .{
                .name = bfd.name,
                .inner_name = bfd.name,
                .type = function_type,
                .binding = .@"const",
                .is_pub = bfd.is_pub,
                .free_inner_name = false,
                .free_type = true,
            });

            const return_type_comp = try c.compileType(alloc, function_type.function.return_type, bfd.return_type.pos());
            defer alloc.free(return_type_comp);

            const param_list_comp = try parameterList(alloc, bfd.parameters, c);
            defer alloc.free(param_list_comp);

            try c.header.function_decls.print(alloc, "{s} {s}{s};", .{
                return_type_comp,
                bfd.name,
                param_list_comp,
            });
        },
        .binding_type_declaration => |btd| {
            try c.module.register(alloc, .{
                .name = btd.name,
                .inner_name = btd.name,
                .type = .type,
                .binding = .@"const",
                .is_pub = btd.is_pub,
                .value = .{
                    .type = switch (btd.type) {
                        inline else => |tag| @unionInit(Type, @tagName(tag), .{
                            .name = btd.name,
                            .members = &.{},
                            .symbols = &.{},
                        }),
                    },
                },
                .free_inner_name = false,
                .free_type = false,
            });
            try c.header.forward_decls.print(alloc, "typedef {s} {s} {1s};", .{
                @tagName(btd.type),
                btd.name,
            });
        },
        .function_definition => |fd| {
            const inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{ c.module.name, fd.name });
            const function_type_ast = try fd.getType(alloc);
            defer function_type_ast.deinit(alloc);

            const function_type: Type = try .fromAst(alloc, &function_type_ast, c);
            try c.module.register(alloc, .{
                .name = fd.name,
                .inner_name = inner_name,
                .type = function_type,
                .binding = .@"const",
                .is_pub = fd.is_pub,
                .free_inner_name = true,
                .free_type = true,
            });

            const return_type_comp = try c.compileType(alloc, function_type.function.return_type, fd.return_type.pos());
            defer alloc.free(return_type_comp);

            try c.module.pushScope(alloc);
            defer c.module.popScope(alloc);

            const param_list_comp = try parameterList(alloc, fd.parameters, c);
            defer alloc.free(param_list_comp);

            try c.header.function_decls.print(alloc, "{s} {s}{s};", .{
                return_type_comp,
                inner_name,
                param_list_comp,
            });

            const body = try block(alloc, fd.body, c);
            defer alloc.free(body);
            try c.source.function_impls.print(alloc, "{s} {s}{s} {s}", .{
                return_type_comp,
                inner_name,
                param_list_comp,
                body,
            });
        },
        .variable_definition => |vd| {
            const def_comp = try variableDefinition(alloc, vd, c);
            defer alloc.free(def_comp);

            const signature = std.mem.indexOfScalar(u8, def_comp, '=') orelse
                std.mem.indexOfScalar(u8, def_comp, ';').?;
            try c.header.variables.print(alloc, "extern {s};", .{def_comp[0..signature]});

            try c.source.variables.appendSlice(alloc, def_comp);
        },
        .struct_declaration => |sd| {
            var typedef: std.ArrayList(u8) = .empty;
            defer typedef.deinit(alloc);

            var members: std.ArrayList(Type.Struct.Member) = .empty;

            const inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{ c.module.name, sd.name });

            try typedef.print(alloc, "typedef struct {s} {{", .{inner_name});
            for (sd.members) |member| {
                const member_t: Type = try .fromAst(alloc, &member.type, c);
                try members.append(alloc, .{
                    .name = try alloc.dupe(u8, member.name),
                    .inner_name = try alloc.dupe(u8, member.name),
                    .type = member_t,
                });

                const t_comp = try c.compileType(alloc, &member_t, member.type.pos());
                defer alloc.free(t_comp);

                try typedef.print(alloc, "{s} {s};\n", .{ t_comp, member.name });
            }
            try typedef.print(alloc, "}} {s};\n", .{inner_name});

            const t: Type = .{
                .@"struct" = .{
                    .name = sd.name,
                    .members = try members.toOwnedSlice(alloc),
                    .symbols = &.{},
                },
            };

            try c.module.register(alloc, .{
                .name = sd.name,
                .inner_name = inner_name,
                .type = .type,
                .binding = .@"const",
                .is_pub = sd.is_pub,
                .value = .{ .type = t },
                .free_type = true,
                .free_inner_name = true,
            });

            try c.header.typedefs.appendSlice(alloc, typedef.items);
        },
        else => {},
    }
}

fn variableDefinition(
    alloc: std.mem.Allocator,
    vd: ast.Statement.VariableDefinition,
    c: *Compiler,
) Error![]const u8 {
    const t: Type = if (vd.type) |*t|
        try .fromAst(alloc, t, c)
    else
        try .infer(alloc, &vd.assigned_value, c);
    errdefer t.deinit(alloc);

    try c.module.register(alloc, .{
        .name = vd.variable_name,
        .type = t,
        .binding = vd.binding,
        .inner_name = vd.variable_name,
        .free_inner_name = false,
        .free_type = true,
    });

    const t_comp = try c.compileType(alloc, &t, if (vd.type) |vdt| vdt.pos() else 0);
    defer alloc.free(t_comp);

    if (vd.assigned_value == .ident and
        std.mem.eql(u8, vd.assigned_value.ident.payload, "undefined"))
        return try std.fmt.allocPrint(alloc, "{s} {s};", .{ t_comp, vd.variable_name });

    const expr_comp = try expressions.compile(alloc, &vd.assigned_value, c, .{
        .is_variable_decl = true,
        .expected_type = if (vd.type) |_| t else null,
    });
    defer alloc.free(expr_comp);

    return try std.fmt.allocPrint(alloc, "{s} {s} = {s};", .{
        t_comp,
        vd.variable_name,
        expr_comp,
    });
}

pub fn block(alloc: std.mem.Allocator, b: ast.Block, c: *Compiler) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(alloc);

    try buf.append(alloc, '{');
    try c.module.pushScope(alloc);
    for (b) |statement| {
        const statement_comp = try compile(alloc, statement, c);
        defer alloc.free(statement_comp);
        try buf.appendSlice(alloc, statement_comp);
    }
    c.module.popScope(alloc);
    try buf.append(alloc, '}');

    return try buf.toOwnedSlice(alloc);
}

fn parameterList(alloc: std.mem.Allocator, parameter_list: ast.ParameterList, c: *Compiler) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(alloc);

    try buf.append(alloc, '(');
    if (parameter_list.len == 0)
        try buf.appendSlice(alloc, "void")
    else for (parameter_list, 0..) |group, i| {
        for (group.names, 0..) |name, j| {
            const param_t: Type = try .fromAst(alloc, &group.type, c);

            try c.module.register(alloc, .{
                .name = name,
                .inner_name = name,
                .type = param_t,
                .binding = if (group.is_mut[j]) .let_mut else .let,
                .free_inner_name = false,
                .free_type = true,
            });

            if (param_t == .variadic) {
                try buf.appendSlice(alloc, "...");
                if (i < parameter_list.len - 1 or j < group.names.len - 1) try buf.append(alloc, ',');
                continue;
            }

            const t_comp = try c.compileType(alloc, &param_t, group.type.pos());
            defer alloc.free(t_comp);

            try buf.print(alloc, "{s} {s}", .{ t_comp, name });
            if (i < parameter_list.len - 1 or j < group.names.len - 1) try buf.append(alloc, ',');
        }
    }
    try buf.append(alloc, ')');

    return buf.toOwnedSlice(alloc);
}

fn import(alloc: std.mem.Allocator, compiler: *Compiler, statement: ast.TopLevelStatement.Import) !void {
    // const mod = void;
    // compiler.module.register(alloc, .{
    //     .name = import.alias orelse import.module_name[import.module_name.len - 1],
    //     .type = .{ .module = mod },
    //     .binding = .@"const",
    // });
    inline for (&.{ compiler.source.includes, compiler.header.includes }) |writer| {
        try writer.appendSlice(alloc, "#include <");
        for (statement.module_name, 0..) |submod, i| {
            try writer.appendSlice(alloc, submod);
            try writer.appendSlice(alloc, if (i == statement.module_name.len - 1) ".h>\n" else "/");
        }
    }
}

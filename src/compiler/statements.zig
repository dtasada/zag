const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const Compiler = @import("compiler.zig").Compiler;
const Type = @import("type.zig").Type;

pub fn compile(alloc: std.mem.Allocator, statement: ast.Statement) ![]const u8 {
    // switch (statement) {}

    _ = alloc;
    _ = statement;
    unreachable;
}

pub fn compileTopLevel(alloc: std.mem.Allocator, statement: ast.TopLevelStatement, compiler: *Compiler) !void {
    switch (statement) {
        .import => |s| try import(alloc, compiler, s),
        .binding_function_declaration => |bfd| {
            var return_type: ?*const Type = try .fromAstPtr(alloc, bfd.return_type, compiler);
            errdefer if (return_type) |rt| rt.deinitPtr(alloc);

            var params: std.ArrayList(Type) = .empty;
            {
                errdefer utils.deinitArrayList(Type, &params, alloc);
                for (bfd.parameters) |param| {
                    const param_type: Type = try .fromAst(alloc, param.type, compiler);
                    errdefer param_type.deinit(alloc);

                    try compiler.module.register(alloc, .{
                        .name = param.name,
                        .inner_name = param.name,
                        .type = param_type,
                        .free_inner_name = false,
                        .free_type = false,
                    });
                    try params.append(alloc, param_type);
                }
            }

            const function_type: Type = .{
                .function = .{
                    .parameters = try params.toOwnedSlice(alloc),
                    .return_type = return_type.?,
                },
            };
            return_type = null;

            errdefer function_type.deinit(alloc);

            try compiler.module.register(alloc, .{
                .name = bfd.name,
                .inner_name = bfd.name,
                .type = function_type,
                .binding = .@"const",
                .is_pub = bfd.is_pub,
                .free_inner_name = false,
                .free_type = true,
            });

            const return_type_comp = try compiler.compileType(alloc, function_type.function.return_type, bfd.return_type.pos());
            defer alloc.free(return_type_comp);

            const param_list_comp = try parameterList(alloc, bfd.parameters, compiler);
            defer alloc.free(param_list_comp);

            try compiler.header.function_decls.print(alloc, "{s} {s}{s};", .{
                return_type_comp,
                bfd.name,
                param_list_comp,
            });
        },
        .binding_type_declaration => |btd| {
            compiler.module.register(alloc, .{
                .name = btd.name,
                .inner_name = btd.name,
                .type = .type,
                .binding = .@"const",
                .is_pub = btd.is_pub,
                .value = .{
                    .type = @unionInit(Type, @tagName(btd.type), .{
                        .name = btd.name,
                        .members = &.{},
                        .symbols = &.{},
                    }),
                },
                .free_inner_name = false,
                .free_type = false,
            });
            try compiler.header.forward_decls.print(alloc, "typedef {s} {s} {1s};", .{
                @tagName(btd.type),
                btd.name,
            });
        },
        else => {},
    }
}

fn parameterList(alloc: std.mem.Allocator, parameter_list: []const ast.VariableSignature, c: *Compiler) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(alloc);

    try buf.append(alloc, '(');
    for (parameter_list, 0..) |param, i| {
        const variable_signature = try variableSignature(alloc, param, c);
        defer alloc.free(variable_signature);
        try buf.appendSlice(alloc, variable_signature);
        try buf.append(alloc, if (i == parameter_list.len - 1) ')' else ',');
    }

    return buf.toOwnedSlice(alloc);
}

fn variableSignature(alloc: std.mem.Allocator, signature: ast.VariableSignature, c: *Compiler) ![]const u8 {
    if (signature.type == .variadic) return try alloc.dupe(u8, "...");

    const t: Type = try .fromAst(alloc, signature.type, c);
    defer t.deinit(alloc);

    const type_comp = try c.compileType(alloc, &t, signature.type.pos());
    defer alloc.free(type_comp);

    return try std.fmt.allocPrint(alloc, "{s} {s}", .{ type_comp, signature.name });
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

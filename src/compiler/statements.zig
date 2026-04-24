const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const expressions = @import("expressions.zig");

const errors = @import("errors.zig");
const compiler = @import("compiler.zig");

const Compiler = compiler.Compiler;
const Type = @import("type.zig").Type;
const Symbol = compiler.Symbol;
const Value = @import("value.zig").Value;
const Error = errors.Error;

pub fn compile(
    alloc: std.mem.Allocator,
    io: std.Io,
    expr: *const ast.Statement,
    c: *Compiler,
) Error![]const u8 {
    return switch (expr.*) {
        .expression => |e| {
            const comp = try expressions.compile(alloc, io, &e, c, .{});
            defer alloc.free(comp);
            return try std.fmt.allocPrint(alloc, "{s};", .{comp});
        },
        .block_eval => |e| {
            const comp = try expressions.compile(alloc, io, &e, c, .{});
            defer alloc.free(comp);
            return try std.fmt.allocPrint(alloc, "{s};", .{comp});
        },
        .variable_definition => |vd| try variableDefinition(alloc, io, vd, c),
        .@"if" => |cond| try @"if"(alloc, io, cond, c),
        .@"while" => |cond| try @"while"(alloc, io, cond, c),
        .@"for" => |cond| try @"for"(alloc, io, cond, c),
        .@"break" => try alloc.dupe(u8, "break;"),
        .@"continue" => try alloc.dupe(u8, "continue;"),
        .@"return" => |ret| {
            // Find the enclosing function definition
            var it = std.mem.reverseIterator(c.module.scopes.items);
            const expected_return_type: Type = b: {
                while (it.next()) |scope| {
                    for (scope.symbols.items) |symbol| {
                        if (symbol.type == .function) {
                            break :b symbol.type.function.return_type.*;
                        }
                    }
                }
                return errors.illegalReturn(io, c.source_map[ret.pos]);
            };

            return try @"return"(alloc, io, ret, c, expected_return_type);
        },
        .block => |b| try block(alloc, io, b.payload, c, .{}),
        .@"defer" => @panic("defer is unimplemented in the compiler"),
    };
}

pub fn compileTopLevel(
    alloc: std.mem.Allocator,
    io: std.Io,
    statement: ast.TopLevelStatement,
    c: *Compiler,
) Error!void {
    switch (statement) {
        .import => |s| try import(alloc, s, c),
        .binding_function_declaration => |bfd| {
            const function_type_ast = try bfd.getType(alloc);
            defer function_type_ast.deinit(alloc);

            const function_type: Type = try .fromAst(alloc, io, &function_type_ast, c);
            const symbol = Symbol{
                .name = bfd.name,
                .inner_name = bfd.name,
                .type = function_type,
                .binding = .@"const",
                .is_pub = bfd.is_pub,
                .free_name = false,
                .free_inner_name = false,
                .free_type = true,
            };
            errdefer symbol.deinit(alloc);
            try c.module.register(alloc, symbol);

            const return_type_comp = try c.compileType(alloc, io, function_type.function.return_type, bfd.return_type.pos());
            defer alloc.free(return_type_comp);

            const param_list_comp = try parameterList(alloc, io, bfd.parameters, c);
            defer alloc.free(param_list_comp);

            try c.header.function_decls.print(alloc, "{s} {s}{s};", .{
                return_type_comp,
                bfd.name,
                param_list_comp,
            });
        },
        .binding_type_declaration => |btd| {
            const symbol = Symbol{
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
                            .tag_type = undefined, // todo: binding unions might need a specified tag_type
                        }),
                    },
                },
                .free_name = false,
                .free_inner_name = false,
                .free_type = false,
            };
            errdefer symbol.deinit(alloc);
            try c.module.register(alloc, symbol);
            try c.header.forward_decls.print(alloc, "typedef {s} {s} {1s};", .{
                @tagName(btd.type),
                btd.name,
            });
        },
        .function_definition => |fd| {
            if (fd.generic_parameters.len > 0) {
                const symbol = Symbol{
                    .name = fd.name,
                    .inner_name = fd.name,
                    .type = .{ .template = .{ .function_definition = try fd.clone(alloc) } },
                    .binding = .@"const",
                    .is_pub = fd.is_pub,
                    .free_name = false,
                    .free_type = true,
                    .free_inner_name = false,
                };
                errdefer symbol.deinit(alloc);
                return try c.module.register(alloc, symbol);
            }

            const inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{ c.module.name, fd.name });
            const function_type_ast = try fd.getType(alloc);
            defer function_type_ast.deinit(alloc);

            const function_type: Type = try .fromAst(alloc, io, &function_type_ast, c);
            const symbol = Symbol{
                .name = fd.name,
                .inner_name = inner_name,
                .type = function_type,
                .binding = .@"const",
                .is_pub = fd.is_pub,
                .free_name = false,
                .free_inner_name = true,
                .free_type = true,
            };
            errdefer symbol.deinit(alloc);
            try c.module.register(alloc, symbol);

            const return_type_comp = try c.compileType(alloc, io, function_type.function.return_type, fd.return_type.pos());
            defer alloc.free(return_type_comp);

            try c.module.pushScope(alloc);
            defer c.module.popScope(alloc);

            const param_list_comp = try parameterList(alloc, io, fd.parameters, c);
            defer alloc.free(param_list_comp);

            try c.header.function_decls.print(alloc, "{s} {s}{s};", .{
                return_type_comp,
                inner_name,
                param_list_comp,
            });

            const body = try block(alloc, io, fd.body, c, .{
                .return_type = function_type.function.return_type.*,
            });
            defer alloc.free(body);
            try c.source.function_impls.print(alloc, "{s} {s}{s} {s}", .{
                return_type_comp,
                inner_name,
                param_list_comp,
                body,
            });
        },
        .variable_definition => |vd| {
            const def_comp = try variableDefinition(alloc, io, vd, c);
            defer alloc.free(def_comp);

            const signature = std.mem.indexOfScalar(u8, def_comp, '=') orelse
                std.mem.indexOfScalar(u8, def_comp, ';').?;
            try c.header.variables.print(alloc, "extern {s};", .{def_comp[0..signature]});

            try c.source.variables.appendSlice(alloc, def_comp);
        },
        inline .struct_declaration, .enum_declaration, .union_declaration => |sd, tag| {
            if (tag != .enum_declaration and sd.generic_types.len > 0) {
                const symbol = Symbol{
                    .name = sd.name,
                    .inner_name = sd.name,
                    .type = .{ .template = @unionInit(Type.Template, @tagName(tag), try sd.clone(alloc)) },
                    .binding = .@"const",
                    .is_pub = sd.is_pub,
                    .free_name = false,
                    .free_type = true,
                    .free_inner_name = false,
                };
                errdefer symbol.deinit(alloc);
                return try c.module.register(alloc, symbol);
            }

            var members_set: std.StringHashMap(usize) = .init(alloc);
            defer members_set.deinit();
            for (sd.members) |m| {
                if (members_set.get(m.name)) |pos|
                    return errors.duplicateStructMember(io, m.name, c.source_map[pos], c.source_map[m.pos]);
                try members_set.put(m.name, m.pos);
            }
            for (sd.variables) |vd| {
                if (members_set.get(vd.variable_name)) |pos|
                    return errors.duplicateStructMember(io, vd.variable_name, c.source_map[pos], c.source_map[vd.pos]);
                try members_set.put(vd.variable_name, vd.pos);
            }
            for (sd.methods) |m| {
                if (members_set.get(m.name)) |pos|
                    return errors.duplicateStructMember(io, m.name, c.source_map[pos], c.source_map[m.pos]);
                try members_set.put(m.name, m.pos);
            }

            const t_tag = switch (tag) {
                .struct_declaration => .@"struct",
                .union_declaration => .@"union",
                .enum_declaration => .@"enum",
                else => unreachable,
            };

            var tag_type: ?*Type = null;
            if (tag == .union_declaration) {
                tag_type = try alloc.create(Type);
                tag_type.?.* = .u8; // todo: smallest int for
            }

            const inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{ c.module.name, sd.name });

            const symbol = try alloc.create(Symbol);
            symbol.* = .{
                .name = try alloc.dupe(u8, sd.name),
                .inner_name = inner_name,
                .type = .type,
                .binding = .@"const",
                .is_pub = sd.is_pub,
                .value = .{
                    .type = switch (tag) {
                        .struct_declaration => .{ .@"struct" = .{
                            .name = try alloc.dupe(u8, sd.name),
                            .members = &.{},
                            .symbols = &.{},
                            .tag_type = tag_type,
                        } },
                        .union_declaration => .{ .@"union" = .{
                            .name = try alloc.dupe(u8, sd.name),
                            .members = &.{},
                            .symbols = &.{},
                            .tag_type = tag_type,
                        } },
                        .enum_declaration => .{ .@"enum" = .{
                            .name = try alloc.dupe(u8, sd.name),
                            .members = &.{},
                            .symbols = &.{},
                            .tag_type = tag_type,
                        } },
                        else => unreachable,
                    },
                },
                .free_name = true,
                .free_type = true,
                .free_inner_name = true,
            };
            c.module.registerPtr(alloc, symbol) catch |err| {
                symbol.deinit(alloc);
                alloc.destroy(symbol);
                return err;
            };

            try c.module.pushScope(alloc);
            defer c.module.popScope(alloc);

            const ct_type = Type.CompoundType(t_tag);
            var members: std.ArrayList(ct_type.Member) = .empty;
            defer {
                for (members.items) |m| m.deinit(alloc);
                members.deinit(alloc);
            }

            var symbols: std.ArrayList(Symbol) = .empty;
            defer {
                for (symbols.items) |s| s.deinit(alloc);
                symbols.deinit(alloc);
            }

            var typedef: std.ArrayList(u8) = .empty;
            defer typedef.deinit(alloc);

            switch (tag) {
                .struct_declaration => {
                    try typedef.print(alloc, "typedef struct {s} {{", .{symbol.inner_name});
                    for (sd.members) |member| {
                        const member_t: Type = try .fromAst(alloc, io, &member.type, c);
                        try members.append(alloc, .{
                            .name = try alloc.dupe(u8, member.name),
                            .inner_name = try alloc.dupe(u8, member.name),
                            .type = member_t,
                        });

                        const t_comp = try c.compileType(alloc, io, &member_t, member.type.pos());
                        defer alloc.free(t_comp);

                        try typedef.print(alloc, "{s} {s};", .{ t_comp, member.name });
                    }
                    try typedef.print(alloc, "}} {s};\n", .{symbol.inner_name});
                },
                .union_declaration => {
                    const tag_type_comp = try c.compileType(alloc, io, symbol.value.?.type.@"union".tag_type.?, sd.pos);
                    defer alloc.free(tag_type_comp);

                    try typedef.print(alloc, "typedef struct {s} {{", .{symbol.inner_name});
                    try typedef.print(alloc, "{s} tag;\n", .{tag_type_comp});
                    try typedef.print(alloc, "union {{\n", .{});
                    for (sd.members) |member| {
                        const member_t: Type = if (member.type) |*t| try .fromAst(alloc, io, t, c) else .u8;
                        try members.append(alloc, .{
                            .name = try alloc.dupe(u8, member.name),
                            .inner_name = try alloc.dupe(u8, member.name),
                            .type = member_t,
                        });

                        const t_comp = try c.compileType(alloc, io, &member_t, if (member.type) |t| t.pos() else 0);
                        defer alloc.free(t_comp);

                        try typedef.print(alloc, "{s} {s};\n", .{ t_comp, member.name });
                    }
                    try typedef.print(alloc, "}} payload;\n", .{});
                    try typedef.print(alloc, "}} {s};\n", .{symbol.inner_name});
                },
                .enum_declaration => {
                    try typedef.print(alloc, "typedef enum {s} {{", .{symbol.inner_name});
                    for (sd.members, 0..) |member, i| {
                        const value: Value = if (member.value) |*v|
                            try .eval(v, c)
                        else
                            .{ .uint = if (i == 0) 0 else members.items[i - 1].value + 1 };

                        if (value != .uint)
                            return errors.enumMemberMustBeInteger(io, value.getType(), c.source_map[sd.pos]);

                        const m_inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{
                            symbol.inner_name,
                            member.name,
                        });
                        try members.append(alloc, .{
                            .name = try alloc.dupe(u8, member.name),
                            .inner_name = m_inner_name,
                            .value = value.uint,
                        });

                        try typedef.print(alloc, "{s} = {},\n", .{ m_inner_name, value.uint });
                    }
                    try typedef.print(alloc, "}} {s};\n", .{symbol.inner_name});
                },
                else => unreachable,
            }

            const ct_ptr = switch (tag) {
                .struct_declaration => &symbol.value.?.type.@"struct",
                .union_declaration => &symbol.value.?.type.@"union",
                .enum_declaration => &symbol.value.?.type.@"enum",
                else => unreachable,
            };
            ct_ptr.members = try members.toOwnedSlice(alloc);

            for (sd.methods) |method| {
                if (method.generic_parameters.len > 0) continue;

                const m_inner_name = try std.fmt.allocPrint(alloc, "{s}_{s}", .{
                    symbol.inner_name,
                    method.name,
                });

                const method_t_ast = try method.getType(alloc);
                defer method_t_ast.deinit(alloc);
                const t: Type = try .fromAst(alloc, io, &method_t_ast, c);
                const m_symbol = try alloc.create(Symbol);
                m_symbol.* = .{
                    .name = try alloc.dupe(u8, method.name),
                    .inner_name = m_inner_name,
                    .type = t,
                    .binding = .@"const",
                    .is_pub = method.is_pub,
                    .free_name = true,
                    .free_type = true,
                    .free_inner_name = true,
                };
                c.module.registerPtr(alloc, m_symbol) catch |err| {
                    m_symbol.deinit(alloc);
                    alloc.destroy(m_symbol);
                    return err;
                };

                const cloned = try m_symbol.clone(alloc);
                try symbols.append(alloc, cloned);

                const return_t: Type = try .fromAst(alloc, io, &method.return_type, c);
                defer return_t.deinit(alloc);

                const return_comp = try c.compileType(alloc, io, &return_t, method.return_type.pos());
                defer alloc.free(return_comp);

                const params_comp = try parameterList(alloc, io, method.parameters, c);
                defer alloc.free(params_comp);

                const body_comp = try block(alloc, io, method.body, c, .{ .return_type = return_t });
                defer alloc.free(body_comp);

                try c.source.function_impls.print(alloc, "{s} {s}{s} {s}", .{
                    return_comp,
                    m_symbol.inner_name,
                    params_comp,
                    body_comp,
                });
            }

            ct_ptr.symbols = try symbols.toOwnedSlice(alloc);

            try c.header.typedefs.appendSlice(alloc, typedef.items);
        },
    }
}

fn import(alloc: std.mem.Allocator, s: ast.TopLevelStatement.Import, c: *Compiler) Error!void {
    var path_buf: [1024]u8 = undefined;
    var path: []const u8 = "";
    for (s.module_name) |name| {
        path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ path, name }) catch |err| switch (err) {
            error.NoSpaceLeft => return error.NoSpaceLeft,
        };
    }
    const full_path = try std.fmt.allocPrint(alloc, "{s}.zag", .{path[1..]});
    defer alloc.free(full_path);

    compiler.emit(alloc, c.io, full_path) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.UnknownSymbol, // todo: better error for emit failure
    };
}

pub fn parameterList(
    alloc: std.mem.Allocator,
    io: std.Io,
    parameter_list: ast.ParameterList,
    c: *Compiler,
) Error![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(alloc);

    try buf.append(alloc, '(');
    if (parameter_list.len == 0)
        try buf.appendSlice(alloc, "void")
    else for (parameter_list, 0..) |group, i| {
        for (group.names, 0..) |name, j| {
            const param_t: Type = try .fromAst(alloc, io, &group.type, c);
            const symbol = Symbol{
                .name = name,
                .inner_name = name,
                .type = param_t,
                .binding = if (group.is_mut[j]) .let_mut else .let,
                .free_name = false,
                .free_inner_name = false,
                .free_type = true,
            };
            errdefer symbol.deinit(alloc);
            try c.module.register(alloc, symbol);

            if (param_t == .variadic) {
                try buf.appendSlice(alloc, "...");
                if (i < parameter_list.len - 1 or j < group.names.len - 1) try buf.append(alloc, ',');
                continue;
            }

            const t_comp = try c.compileType(alloc, io, &param_t, group.type.pos());
            defer alloc.free(t_comp);
            try buf.print(alloc, "{s} {s}", .{ t_comp, name });
            if (i < parameter_list.len - 1 or j < group.names.len - 1) try buf.append(alloc, ',');
        }
    }
    try buf.append(alloc, ')');

    return try buf.toOwnedSlice(alloc);
}

pub fn variableDefinition(
    alloc: std.mem.Allocator,
    io: std.Io,
    vd: ast.Statement.VariableDefinition,
    c: *Compiler,
) Error![]const u8 {
    const t: Type = if (vd.type) |*t|
        try .fromAst(alloc, io, t, c)
    else
        try .infer(alloc, io, &vd.assigned_value, c);

    const symbol = Symbol{
        .name = vd.variable_name,
        .type = t,
        .binding = vd.binding,
        .inner_name = vd.variable_name,
        .free_name = false,
        .free_inner_name = false,
        .free_type = true,
    };
    errdefer symbol.deinit(alloc);
    try c.module.register(alloc, symbol);

    const t_comp = try c.compileType(alloc, io, &t, if (vd.type) |vdt| vdt.pos() else 0);
    defer alloc.free(t_comp);

    if (vd.assigned_value == .ident and
        std.mem.eql(u8, vd.assigned_value.ident.payload, "undefined"))
        return try std.fmt.allocPrint(alloc, "{s} {s};", .{ t_comp, vd.variable_name });

    const expr_comp = try expressions.compile(alloc, io, &vd.assigned_value, c, .{
        .expected_type = if (vd.type) |_| t else null,
    });
    defer alloc.free(expr_comp);

    return try std.fmt.allocPrint(alloc, "{s} {s} = {s};", .{ t_comp, vd.variable_name, expr_comp });
}

pub fn @"if"(
    alloc: std.mem.Allocator,
    io: std.Io,
    cond: ast.Statement.If,
    c: *Compiler,
) Error![]const u8 {
    const cond_t = try Type.infer(alloc, io, &cond.condition, c);
    defer cond_t.deinit(alloc);
    if (cond_t != .bool)
        return errors.typeMismatch(io, .bool, cond_t, c.source_map[cond.condition.pos()]);

    const cond_comp = try expressions.compile(alloc, io, &cond.condition, c, .{});
    defer alloc.free(cond_comp);

    try c.module.pushScope(alloc);
    defer c.module.popScope(alloc);

    if (cond.capture) |cap| {
        const symbol = Symbol{
            .name = cap.name,
            .inner_name = cap.name,
            .type = .bool, // todo: actual type of capture
            .binding = if (cap.takes_ref != .none) .let_mut else .let,
            .free_name = false,
            .free_inner_name = false,
            .free_type = false,
        };
        errdefer symbol.deinit(alloc);
        try c.module.register(alloc, symbol);
    }

    const body_comp = try compile(alloc, io, cond.body, c);
    defer alloc.free(body_comp);

    if (cond.@"else") |e| {
        const else_comp = try compile(alloc, io, e, c);
        defer alloc.free(else_comp);
        return try std.fmt.allocPrint(alloc, "if ({s}) {s} else {s}", .{ cond_comp, body_comp, else_comp });
    }

    return try std.fmt.allocPrint(alloc, "if ({s}) {s}", .{ cond_comp, body_comp });
}

pub fn @"while"(
    alloc: std.mem.Allocator,
    io: std.Io,
    cond: ast.Statement.While,
    c: *Compiler,
) Error![]const u8 {
    const cond_t = try Type.infer(alloc, io, &cond.condition, c);
    defer cond_t.deinit(alloc);
    if (cond_t != .bool)
        return errors.typeMismatch(io, .bool, cond_t, c.source_map[cond.condition.pos()]);

    const cond_comp = try expressions.compile(alloc, io, &cond.condition, c, .{});
    defer alloc.free(cond_comp);

    try c.module.pushScope(alloc);
    defer c.module.popScope(alloc);

    if (cond.capture) |cap| {
        const symbol = Symbol{
            .name = cap.name,
            .inner_name = cap.name,
            .type = .bool, // todo: actual type of capture
            .binding = if (cap.takes_ref != .none) .let_mut else .let,
            .free_name = false,
            .free_inner_name = false,
            .free_type = false,
        };
        errdefer symbol.deinit(alloc);
        try c.module.register(alloc, symbol);
    }

    const body_comp = try compile(alloc, io, cond.body, c);
    defer alloc.free(body_comp);

    return try std.fmt.allocPrint(alloc, "while ({s}) {s}", .{ cond_comp, body_comp });
}

pub fn @"for"(
    alloc: std.mem.Allocator,
    io: std.Io,
    cond: ast.Statement.For,
    c: *Compiler,
) Error![]const u8 {
    const iter_t = try Type.infer(alloc, io, &cond.iterator, c);
    defer iter_t.deinit(alloc);

    const iter_comp = try expressions.compile(alloc, io, &cond.iterator, c, .{});
    defer alloc.free(iter_comp);

    try c.module.pushScope(alloc);
    defer c.module.popScope(alloc);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(alloc);

    const i_name = try std.fmt.allocPrint(alloc, "_{x}", .{cond.pos});
    defer alloc.free(i_name);

    switch (iter_t) {
        .slice => |s| {
            if (cond.capture) |cap| {
                const symbol = Symbol{
                    .name = cap.name,
                    .inner_name = try std.fmt.allocPrint(alloc, "{s}[{s}]", .{ iter_comp, i_name }),
                    .type = s.inner.*,
                    .binding = if (cap.takes_ref != .none) .let_mut else .let,
                    .free_name = false,
                    .free_inner_name = true,
                    .free_type = false,
                };
                errdefer symbol.deinit(alloc);
                try c.module.register(alloc, symbol);
            }

            try buf.print(alloc, "for (size_t {s} = 0; {0s} < {s}.len; {0s}++) ", .{
                i_name,
                iter_comp,
            });
        },
        .array => |s| {
            if (cond.capture) |cap| {
                const symbol = Symbol{
                    .name = cap.name,
                    .inner_name = try std.fmt.allocPrint(alloc, "{s}[{s}]", .{ iter_comp, i_name }),
                    .type = s.inner.*,
                    .binding = if (cap.takes_ref != .none) .let_mut else .let,
                    .free_name = false,
                    .free_inner_name = true,
                    .free_type = false,
                };
                errdefer symbol.deinit(alloc);
                try c.module.register(alloc, symbol);
            }

            try buf.print(alloc, "for (size_t {s} = 0; {0s} < {}; {0s}++) ", .{
                i_name,
                s.len,
            });
        },
        else => return errors.typeNotIterable(io, iter_t, c.source_map[cond.iterator.pos()]),
    }

    const body_comp = try compile(alloc, io, cond.body, c);
    defer alloc.free(body_comp);

    try buf.appendSlice(alloc, body_comp);

    return try buf.toOwnedSlice(alloc);
}

pub fn @"return"(
    alloc: std.mem.Allocator,
    io: std.Io,
    ret: ast.Statement.Return,
    c: *Compiler,
    expected_type: Type,
) Error![]const u8 {
    const received: Type = if (ret.@"return") |*r| try .infer(alloc, io, r, c) else .void;
    defer received.deinit(alloc);

    if (!received.check(expected_type))
        return errors.typeMismatch(io, expected_type, received, c.source_map[ret.pos]);

    const block_return: ?[]const u8 = if (ret.@"return") |*r| try expressions.compile(alloc, io, r, c, .{
        .expected_type = expected_type,
    }) else null;
    defer if (block_return) |br| alloc.free(br);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(alloc);

    // Deinit all local variables in all scopes except the global one
    for (1..c.module.scopes.items.len) |j| {
        const scope = c.module.scopes.items[c.module.scopes.items.len - j];
        for (scope.defers.items) |d| {
            const st = try compile(alloc, io, &d, c);
            defer alloc.free(st);
            try buf.appendSlice(alloc, st);
        }
    }

    if (block_return) |br| try buf.print(alloc, "return {s};", .{br}) else try buf.appendSlice(alloc, "return;");

    return try buf.toOwnedSlice(alloc);
}

pub fn block(
    alloc: std.mem.Allocator,
    io: std.Io,
    b: ast.Block,
    c: *Compiler,
    opts: struct {
        return_type: ?Type = null,
        eval_type: ?Type = null,
        create_scope: bool = true,
    },
) Error![]const u8 {
    if (opts.create_scope) {
        try c.module.pushScope(alloc);
    }
    defer if (opts.create_scope) c.module.popScope(alloc);

    var block_return: std.ArrayList(u8) = .empty;
    defer block_return.deinit(alloc);

    try block_return.appendSlice(alloc, "{ ");

    const ret_name: ?usize = if (opts.eval_type != null) b[b.len - 1].pos() else null;
    if (ret_name) |rn| {
        const t_comp = try c.compileType(alloc, io, &opts.eval_type.?, rn);
        defer alloc.free(t_comp);
        try block_return.print(alloc, "{s} _{x}; ", .{ t_comp, rn });
    }

    for (b, 0..) |statement, i| {
        if (opts.eval_type != null and i == b.len - 1) {
            const expr_comp = try expressions.compile(alloc, io, &statement.block_eval, c, .{
                .expected_type = opts.eval_type,
            });
            defer alloc.free(expr_comp);
            try block_return.print(alloc, "_{x} = {s}; ", .{ ret_name.?, expr_comp });
        } else if (opts.return_type != null and i == b.len - 1 and statement == .block_eval) {
            const ret = ast.Statement.Return{
                .pos = statement.pos(),
                .@"return" = statement.block_eval,
            };
            const st = try @"return"(alloc, io, ret, c, opts.return_type.?);
            defer alloc.free(st);
            try block_return.appendSlice(alloc, st);
        } else {
            const st = try compile(alloc, io, &statement, c);
            defer alloc.free(st);
            try block_return.appendSlice(alloc, st);
        }
    }
    if (ret_name) |rn| try block_return.print(alloc, "_{x};", .{rn});
    try block_return.appendSlice(alloc, " }");

    return block_return.toOwnedSlice(alloc);
}

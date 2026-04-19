const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const expressions = @import("expressions.zig");

const errors = @import("errors.zig");
const compiler = @import("compiler.zig");
const Error = errors.Error;
const Compiler = compiler.Compiler;
const Symbol = compiler.Symbol;
const Type = @import("type.zig").Type;

pub fn compile(
    alloc: std.mem.Allocator,
    io: std.Io,
    statement: ast.Statement,
    c: *Compiler,
) Error![]const u8 {
    return switch (statement) {
        inline .@"break", .@"continue" => |_, tag| try alloc.dupe(u8, @tagName(tag)),
        inline .@"for", .@"while", .@"if" => |cond, tag| try conditional(
            alloc,
            io,
            std.meta.stringToEnum(utils.ConditionalTag, @tagName(tag)).?,
            cond,
            c,
        ),
        .expression => |*expr| {
            const expr_comp = try expressions.compile(alloc, io, expr, c, .{});
            defer alloc.free(expr_comp);
            return try std.fmt.allocPrint(alloc, "{s};", .{expr_comp});
        },
        .block => |b| block(alloc, io, b.payload, c, .{}),
        .variable_definition => |vd| try variableDefinition(alloc, io, vd, c),
        .block_eval => |eval| return errors.illegalReturn(io, c.source_map[eval.pos()]),
        .@"return" => |ret| return errors.illegalReturn(io, c.source_map[ret.pos]),
        .@"defer" => |def| {
            try c.module.scopes.getLast().defers.append(alloc, def.payload.*);
            return alloc.dupe(u8, "");
        },
    };
}

fn conditional(
    alloc: std.mem.Allocator,
    io: std.Io,
    comptime tag: utils.ConditionalTag,
    statement: switch (tag) {
        .@"for" => ast.Statement.For,
        .@"while" => ast.Statement.While,
        .@"if" => ast.Statement.If,
    },
    c: *Compiler,
) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(alloc);

    if (tag != .@"for") {
        const condition_t: Type = try .infer(alloc, io, &statement.condition, c);
        condition_t.deinit(alloc);
        if (condition_t != .bool and condition_t != .optional)
            return errors.illegalCondition(io, condition_t, c.source_map[statement.condition.pos()]);

        const condition_t_comp = try c.compileType(alloc, io, &condition_t, statement.condition.pos());
        defer alloc.free(condition_t_comp);

        const condition = try expressions.compile(alloc, io, &statement.condition, c, .{});
        defer alloc.free(condition);

        const capture: ?struct {
            first_eval: []const u8,
            eval_name: []const u8,
            capture: []const u8,
            capture_name: []const u8,
            capture_t: Type,
        } =
            if (statement.capture) |capture| b: {
                if (condition_t != .optional)
                    return errors.illegalCapture(io, c.source_map[statement.pos]);

                if (capture.index) |_|
                    return errors.illegalIndexCapture(io, c.source_map[statement.pos]);

                const capture_t: Type = if (statement.capture.?.takes_ref == .some) .{
                    .reference = .{
                        .inner = condition_t.optional,
                        .is_mut = statement.capture.?.takes_ref.some,
                    },
                } else condition_t.optional.*;
                const t_comp = try c.compileType(alloc, io, &capture_t, statement.condition.pos());
                defer alloc.free(t_comp);

                const name = try std.fmt.allocPrint(alloc, "{s}_{x}", .{
                    capture.name,
                    std.hash.Wyhash.hash(0, condition),
                });

                const capture_name = try std.fmt.allocPrint(alloc, "{s}_{x}_capture", .{
                    capture.name,
                    std.hash.Wyhash.hash(0, condition),
                });
                break :b .{
                    .first_eval = try std.fmt.allocPrint(alloc, "{s} {s} = {s};", .{
                        condition_t_comp,
                        name,
                        condition,
                    }),
                    .capture = try std.fmt.allocPrint(alloc, "{s} {s} = {s}{s}.payload;", .{
                        t_comp,
                        capture_name,
                        if (statement.capture.?.takes_ref == .some) "&" else "",
                        condition,
                    }),
                    .eval_name = name,
                    .capture_name = capture_name,
                    .capture_t = capture_t,
                };
            } else null;

        defer if (capture) |cap| {
            alloc.free(cap.first_eval);
            alloc.free(cap.capture);
            alloc.free(cap.eval_name);
            alloc.free(cap.capture_name);
        };

        if (capture) |cap| {
            try buf.appendSlice(alloc, cap.first_eval);
            try buf.appendSlice(alloc, cap.capture);
            try buf.print(alloc, "{s} ({s}.is_some)", .{ @tagName(tag), cap.eval_name });

            const capture_t = try c.compileType(alloc, io, condition_t.optional, statement.pos);
            defer alloc.free(capture_t);

            try c.module.register(alloc, .{
                .name = statement.capture.?.name,
                .type = cap.capture_t,
                .inner_name = cap.capture_name,
                .binding = .let,
                .free_inner_name = false,
                .free_type = false,
            });

            const body = try block(alloc, io, &.{statement.body.*}, c, .{});
            defer alloc.free(body);
            try buf.appendSlice(alloc, body);
        } else {
            try buf.print(alloc, "{s} ({s})", .{ @tagName(tag), condition });
            const body = try block(alloc, io, &.{statement.body.*}, c, .{});
            defer alloc.free(body);
            try buf.appendSlice(alloc, body);
        }
    } else {}

    return buf.toOwnedSlice(alloc);
}

pub fn compileTopLevel(alloc: std.mem.Allocator, io: std.Io, statement: ast.TopLevelStatement, c: *Compiler) !void {
    switch (statement) {
        .import => |s| try import(alloc, s, c),
        .binding_function_declaration => |bfd| {
            const function_type_ast = try bfd.getType(alloc);
            defer function_type_ast.deinit(alloc);

            const function_type: Type = try .fromAst(alloc, io, &function_type_ast, c);
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

            const function_type: Type = try .fromAst(alloc, io, &function_type_ast, c);
            try c.module.register(alloc, .{
                .name = fd.name,
                .inner_name = inner_name,
                .type = function_type,
                .binding = .@"const",
                .is_pub = fd.is_pub,
                .free_inner_name = true,
                .free_type = true,
            });

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
        .struct_declaration => |sd| {
            if (sd.generic_types.len > 0) return;

            var inner_name: ?[]const u8 = try std.fmt.allocPrint(alloc, "{s}_{s}", .{ c.module.name, sd.name });
            defer if (inner_name) |in| alloc.free(in);

            var members: ?std.ArrayList(Type.Struct.Member) = try .initCapacity(alloc, sd.members.len);
            var symbols: ?std.ArrayList(Symbol) = std.ArrayList(Symbol).initCapacity(alloc, sd.methods.len) catch |err| {
                members.?.deinit(alloc);
                return err;
            };

            const symbol = alloc.create(Symbol) catch |err| {
                members.?.deinit(alloc);
                symbols.?.deinit(alloc);
                return err;
            };
            symbol.* = .{
                .name = sd.name,
                .inner_name = inner_name.?,
                .type = .type,
                .binding = .@"const",
                .is_pub = sd.is_pub,
                .value = .{
                    .type = .{
                        .@"struct" = .{
                            .name = try alloc.dupe(u8, sd.name),
                            .members = members.?.items,
                            .symbols = symbols.?.items,
                        },
                    },
                },
                .free_type = true,
                .free_inner_name = true,
            };
            inner_name = null;
            try c.module.registerPtr(alloc, symbol);

            try c.module.pushScope(alloc);
            defer c.module.popScope(alloc);

            var typedef: std.ArrayList(u8) = .empty;
            defer typedef.deinit(alloc);
            try typedef.print(alloc, "typedef struct {s} {{", .{symbol.inner_name});
            for (sd.members) |member| {
                const member_t: Type = try .fromAst(alloc, io, &member.type, c);
                members.?.appendAssumeCapacity(.{
                    .name = try alloc.dupe(u8, member.name),
                    .inner_name = try alloc.dupe(u8, member.name),
                    .type = member_t,
                });
                symbol.value.?.type.@"struct".members = members.?.items;

                const t_comp = try c.compileType(alloc, io, &member_t, member.type.pos());
                defer alloc.free(t_comp);

                try typedef.print(alloc, "{s} {s};\n", .{ t_comp, member.name });
            }
            symbol.value.?.type.@"struct".members = try members.?.toOwnedSlice(alloc);
            members = null;
            try typedef.print(alloc, "}} {s};\n", .{symbol.inner_name});

            for (sd.methods) |method| {
                if (method.generic_parameters.len > 0) continue;

                var m_inner_name: ?[]const u8 = try std.fmt.allocPrint(alloc, "{s}_{s}", .{
                    symbol.inner_name,
                    method.name,
                });
                errdefer if (m_inner_name) |min| alloc.free(min);

                const method_t_ast = try method.getType(alloc);
                defer method_t_ast.deinit(alloc);
                const t: Type = try .fromAst(alloc, io, &method_t_ast, c);
                const m_symbol = try alloc.create(Symbol);
                m_symbol.* = .{
                    .name = method.name,
                    .inner_name = m_inner_name.?,
                    .type = t,
                    .binding = .@"const",
                    .is_pub = method.is_pub,
                    .free_type = true,
                    .free_inner_name = true,
                };
                m_inner_name = null;
                try c.module.registerPtr(alloc, m_symbol);

                symbols.?.appendAssumeCapacity(try m_symbol.clone(alloc));
                symbol.value.?.type.@"struct".symbols = symbols.?.items;

                const return_t: Type = try .fromAst(alloc, io, &method.return_type, c);
                defer return_t.deinit(alloc);

                const return_comp = try c.compileType(alloc, io, &return_t, method.return_type.pos());
                defer alloc.free(return_comp);

                const params_comp = try parameterList(alloc, io, method.parameters, c);
                defer alloc.free(params_comp);

                const body_comp = if (method.body.len == 1 and method.body[0] == .block_eval) b: {
                    const expr_comp = try expressions.compile(
                        alloc,
                        io,
                        &method.body[0].block_eval,
                        c,
                        .{ .expected_type = return_t },
                    );
                    defer alloc.free(expr_comp);
                    break :b try std.fmt.allocPrint(alloc, "{{ return {s}; }}", .{expr_comp});
                } else try block(alloc, io, method.body, c, .{ .return_type = return_t });
                defer alloc.free(body_comp);

                try c.header.function_decls.print(alloc, "{s} {s}{s};", .{
                    return_comp,
                    m_symbol.inner_name,
                    params_comp,
                });

                try c.source.function_impls.print(alloc, "{s} {s}{s} {s}", .{
                    return_comp,
                    m_symbol.inner_name,
                    params_comp,
                    body_comp,
                });
            }
            symbol.value.?.type.@"struct".symbols = try symbols.?.toOwnedSlice(alloc);
            symbols = null;

            try c.header.typedefs.appendSlice(alloc, typedef.items);
        },
        else => {},
    }
}

fn variableDefinition(
    alloc: std.mem.Allocator,
    io: std.Io,
    vd: ast.Statement.VariableDefinition,
    c: *Compiler,
) Error![]const u8 {
    const t: Type = if (vd.type) |*t|
        try .fromAst(alloc, io, t, c)
    else
        try .infer(alloc, io, &vd.assigned_value, c);

    try c.module.register(alloc, .{
        .name = vd.variable_name,
        .type = t,
        .binding = vd.binding,
        .inner_name = vd.variable_name,
        .free_inner_name = false,
        .free_type = true,
    });

    const t_comp = try c.compileType(alloc, io, &t, if (vd.type) |vdt| vdt.pos() else 0);
    defer alloc.free(t_comp);

    if (vd.assigned_value == .ident and
        std.mem.eql(u8, vd.assigned_value.ident.payload, "undefined"))
        return try std.fmt.allocPrint(alloc, "{s} {s};", .{ t_comp, vd.variable_name });

    const expr_comp = try expressions.compile(alloc, io, &vd.assigned_value, c, .{
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

pub fn block(
    alloc: std.mem.Allocator,
    io: std.Io,
    b: ast.Block,
    c: *Compiler,
    opts: struct {
        return_type: ?Type = null,
        eval_type: ?Type = null,
    },
) Error![]const u8 {
    // return_type and eval_type may not both be non-null at once.
    std.debug.assert(opts.return_type == null and opts.eval_type == null or
        opts.return_type == null and opts.eval_type != null or
        opts.eval_type == null and opts.return_type != null);

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(alloc);

    try buf.append(alloc, '{');
    try c.module.pushScope(alloc);
    defer c.module.popScope(alloc);
    var returned: union(enum) {
        yes: ast.Statement.Return,
        block_eval: ast.Expression,
        no,
        @"continue",
        @"break",
    } = .no;
    for (b) |statement| switch (statement) {
        .@"return" => |r| {
            if (returned == .yes)
                return errors.doubleReturn(io, c.source_map[returned.yes.pos], c.source_map[r.pos]);
            if (returned == .block_eval)
                return errors.doubleReturn(io, c.source_map[returned.block_eval.pos()], c.source_map[r.pos]);
            returned = .{ .yes = r };
        },
        .block_eval => |be| {
            if (returned == .yes)
                return errors.doubleReturn(io, c.source_map[returned.yes.pos], c.source_map[be.pos()]);
            if (returned == .block_eval)
                return errors.doubleReturn(io, c.source_map[returned.block_eval.pos()], c.source_map[be.pos()]);

            if (try Type.infer(alloc, io, &be, c) != .void) {
                returned = .{ .block_eval = be };
            } else {
                const statement_comp = try compile(alloc, io, statement, c);
                defer alloc.free(statement_comp);
                try buf.appendSlice(alloc, statement_comp);
            }
        },
        .@"continue" => returned = .@"continue",
        .@"break" => returned = .@"break",
        else => {
            const statement_comp = try compile(alloc, io, statement, c);
            defer alloc.free(statement_comp);
            try buf.appendSlice(alloc, statement_comp);
        },
    };

    switch (returned) {
        .yes => |r| {
            const ret_comp = try @"return"(alloc, io, r, c, opts.return_type orelse
                return errors.illegalReturn(io, c.source_map[r.pos]));
            defer alloc.free(ret_comp);
            try buf.appendSlice(alloc, ret_comp);
        },
        .block_eval => |*be| {
            const expected = opts.eval_type orelse
                return errors.illegalReturn(io, c.source_map[be.pos()]);
            const received: Type = try .infer(alloc, io, be, c);
            defer received.deinit(alloc);

            if (!received.check(expected))
                return errors.typeMismatch(io, expected, received, c.source_map[be.pos()]);

            const t_comp = try c.compileType(alloc, io, &expected, be.pos());
            defer alloc.free(t_comp);

            const ret_comp = try expressions.compile(alloc, io, be, c, .{ .expected_type = expected });
            defer alloc.free(ret_comp);

            const ret_name = std.hash.Wyhash.hash(0, ret_comp);
            try buf.print(alloc, "{s} _{x} = {s};", .{ t_comp, ret_name, ret_comp });
            for (1..c.module.scopes.items.len + 1) |j| {
                const scope = c.module.scopes.items[c.module.scopes.items.len - j];
                for (scope.defers.items) |d| {
                    const st = try compile(alloc, io, d, c);
                    defer alloc.free(st);
                    try buf.appendSlice(alloc, st);
                }
            }
            try buf.print(alloc, "return _{x};", .{ret_name});
        },
        .no => for (c.module.scopes.getLast().defers.items) |d| {
            const st = try compile(alloc, io, d, c);
            defer alloc.free(st);
            try buf.appendSlice(alloc, st);
        },
        inline .@"break", .@"continue" => |_, t| {
            for (c.module.scopes.getLast().defers.items) |d| {
                const st = try compile(alloc, io, d, c);
                defer alloc.free(st);
                try buf.appendSlice(alloc, st);
            }
            try buf.appendSlice(alloc, @tagName(t));
        },
    }

    try buf.append(alloc, '}');

    return try buf.toOwnedSlice(alloc);
}

fn parameterList(
    alloc: std.mem.Allocator,
    io: std.Io,
    parameter_list: ast.ParameterList,
    c: *Compiler,
) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(alloc);

    try buf.append(alloc, '(');
    if (parameter_list.len == 0)
        try buf.appendSlice(alloc, "void")
    else for (parameter_list, 0..) |group, i| {
        for (group.names, 0..) |name, j| {
            const param_t: Type = try .fromAst(alloc, io, &group.type, c);

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

            const t_comp = try c.compileType(alloc, io, &param_t, group.type.pos());
            defer alloc.free(t_comp);

            try buf.print(alloc, "{s} {s}", .{ t_comp, name });
            if (i < parameter_list.len - 1 or j < group.names.len - 1) try buf.append(alloc, ',');
        }
    }
    try buf.append(alloc, ')');

    return buf.toOwnedSlice(alloc);
}

fn import(alloc: std.mem.Allocator, statement: ast.TopLevelStatement.Import, c: *Compiler) !void {
    // const mod = void;
    // compiler.module.register(alloc, .{
    //     .name = import.alias orelse import.module_name[import.module_name.len - 1],
    //     .type = .{ .module = mod },
    //     .binding = .@"const",
    // });
    inline for (&.{ c.source.includes, c.header.includes }) |writer| {
        try writer.appendSlice(alloc, "#include <");
        for (statement.module_name, 0..) |submod, i| {
            try writer.appendSlice(alloc, submod);
            try writer.appendSlice(alloc, if (i == statement.module_name.len - 1) ".h>\n" else "/");
        }
    }
}

pub fn @"return"(
    alloc: std.mem.Allocator,
    io: std.Io,
    ret: ast.Statement.Return,
    c: *Compiler,
    expected_type: Type,
) ![]const u8 {
    const received: Type = if (ret.@"return") |*r| try .infer(alloc, io, r, c) else .void;
    defer received.deinit(alloc);

    if (!received.check(expected_type))
        return errors.typeMismatch(io, expected_type, received, c.source_map[ret.pos]);

    const expr_comp = if (ret.@"return") |*r|
        try expressions.compile(alloc, io, r, c, .{ .expected_type = expected_type })
    else
        null;
    defer if (expr_comp) |ec| alloc.free(ec);

    var block_return: std.ArrayList(u8) = .empty;
    if (expr_comp) |_| try block_return.appendSlice(alloc, "return ({");
    var ret_name: ?u64 = null;
    if (ret.@"return") |*r| {
        const return_type: Type = try .infer(alloc, io, r, c);
        defer return_type.deinit(alloc);
        const return_type_comp = try c.compileType(alloc, io, &return_type, r.pos());
        defer alloc.free(return_type_comp);
        ret_name = std.hash.Wyhash.hash(0, expr_comp.?);
        try block_return.print(alloc, "{s} _{x} = {s};", .{ return_type_comp, ret_name.?, expr_comp.? });
    }
    for (1..c.module.scopes.items.len + 1) |j| {
        const scope = c.module.scopes.items[c.module.scopes.items.len - j];
        for (scope.defers.items) |d| {
            const st = try compile(alloc, io, d, c);
            defer alloc.free(st);
            try block_return.appendSlice(alloc, st);
        }
    }
    if (ret_name) |rn| try block_return.print(alloc, "_{x};}});", .{rn});

    return block_return.toOwnedSlice(alloc);
}

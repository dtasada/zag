const std = @import("std");
const ast = @import("ast");

const errors = @import("errors.zig");
const statements = @import("statements.zig");
const compiler = @import("compiler.zig");
const Compiler = compiler.Compiler;
const Module = compiler.Module;
const Type = compiler.Type;
const Value = compiler.Value;
const Error = errors.Error;

pub fn compile(
    alloc: std.mem.Allocator,
    io: std.Io,
    expr: *const ast.Expression,
    c: *Compiler,
    m: *Module,
    opts: struct {
        expected_type: ?Type = null,
    },
) Error![]const u8 {
    if (opts.expected_type) |et| {
        const received: Type = try .infer(alloc, io, expr, c, m);
        defer received.deinit(alloc);

        if (et.eql(received)) return compile(alloc, io, expr, c, m, .{});
        if (!received.check(et)) return errors.typeMismatch(io, et, received, m.source_map[expr.pos()]);

        switch (et) {
            .optional => {
                const expected_comp = try c.compileType(alloc, io, &et, m, expr.pos());
                defer alloc.free(expected_comp);

                const base = try compile(alloc, io, expr, c, m, .{});
                defer alloc.free(base);
                return std.fmt.allocPrint(alloc, "({s}){{ .is_some = true, .payload = {s} }}", .{
                    expected_comp,
                    base,
                });
            },
            else => return compile(alloc, io, expr, c, m, .{}),
        }
    }

    return switch (expr.*) {
        .bad_node => unreachable,
        .ident => |ident| if (c.module.getSymbol(ident.payload)) |symbol|
            try alloc.dupe(u8, symbol.inner_name)
        else
            errors.unknownSymbol(io, ident.payload, m.source_map[ident.pos]),
        .string => |string| try std.fmt.allocPrint(alloc, "\"{s}\"", .{string.payload}),
        .char => |char| try std.fmt.allocPrint(alloc, "'{c}'", .{char.payload}),
        inline .int, .float => |number| try std.fmt.allocPrint(alloc, "{}", .{number.payload}),
        .@"if" => |cond| {
            const condition = try compile(alloc, io, cond.condition, c, m, .{ .expected_type = .bool });
            defer alloc.free(condition);
            const a = try compile(alloc, io, cond.body, c, m, .{});
            defer alloc.free(a);
            const b = try compile(alloc, io, cond.@"else", c, m, .{});
            defer alloc.free(b);
            return std.fmt.allocPrint(alloc, "({s}) ? {s} : {s}", .{ condition, a, b });
        },
        .block => |blck| {
            const block_comp = try statements.block(alloc, io, blck.payload, c, m, .{});
            defer alloc.free(block_comp);
            return try std.fmt.allocPrint(alloc, "({{{s}}})", .{block_comp});
        },
        .generic => |generic| {
            const lhs_t: Type = try .infer(alloc, io, generic.lhs, c, m);
            defer lhs_t.deinit(alloc);

            if (lhs_t == .template and (lhs_t.template.kind == .builtin_cast or lhs_t.template.kind == .builtin_sizeof)) {
                return try alloc.dupe(u8, if (lhs_t.template.kind == .builtin_cast) "cast" else "sizeof");
            }

            const template_name = switch (lhs_t.template.kind) {
                .builtin_cast => "cast",
                .builtin_sizeof => "sizeof",
                inline else => |d| d.name,
            };

            const mangled_name = try Type.getMangledName(alloc, io, template_name, generic.arguments, c, m);
            defer alloc.free(mangled_name);

            const symbol = c.module.getSymbol(mangled_name) orelse return error.InstantiationFailed;
            return try alloc.dupe(u8, symbol.inner_name);
        },
        .array_instantiation => |inst| {
            const inner_ast: Type = try .fromAst(alloc, io, &inst.type, c, m);
            defer inner_ast.deinit(alloc);

            var buf: std.ArrayList(u8) = .empty;
            errdefer buf.deinit(alloc);

            const inner_t = try c.compileType(alloc, io, &inner_ast, m, inst.type.pos());
            defer alloc.free(inner_t);

            const len: Value = if (inst.length.* == .ident and std.mem.eql(u8, inst.length.ident.payload, "_"))
                .{ .uint = inst.contents.len }
            else
                try .eval(alloc, io, inst.length, c, m);

            if (len != .uint)
                return errors.arrayLengthMustBeInteger(io, len.getType(), m.source_map[inst.length.pos()]);

            if (len.uint != inst.contents.len)
                return errors.arrayInstantiationSizeMismatch(io, len.uint, inst.contents.len, m.source_map[inst.length.pos()]);

            const t: Type = .{ .array = .{ .inner = &inner_ast, .len = len.uint } };
            const t_comp = try c.compileType(alloc, io, &t, m, inst.pos);
            defer alloc.free(t_comp);

            try buf.print(alloc, "({s})", .{t_comp});

            try buf.append(alloc, '{');
            for (inst.contents, 0..) |*item, i| {
                const item_comp = try compile(alloc, io, item, c, m, .{});
                defer alloc.free(item_comp);

                try buf.appendSlice(alloc, item_comp);
                try buf.append(alloc, if (i == inst.contents.len - 1) '}' else ',');
            }

            return try buf.toOwnedSlice(alloc);
        },
        .call => |call| {
            if (call.callee.* == .generic) {
                const generic = call.callee.generic;
                const lhs_t: Type = try .infer(alloc, io, generic.lhs, c, m);
                defer lhs_t.deinit(alloc);
                if (lhs_t == .template) {
                    switch (lhs_t.template.kind) {
                        .builtin_cast => {
                            if (call.args.len != 1) return error.TypeMismatch;
                            const target_ast: Type = try .fromAst(alloc, io, &generic.arguments[0].type.payload, c, m);
                            defer target_ast.deinit(alloc);
                            const target_comp = try c.compileType(alloc, io, &target_ast, m, generic.arguments[0].type.pos);
                            defer alloc.free(target_comp);

                            const val_comp = try compile(alloc, io, &call.args[0], c, m, .{});
                            defer alloc.free(val_comp);

                            return try std.fmt.allocPrint(alloc, "({s})({s})", .{ target_comp, val_comp });
                        },
                        .builtin_sizeof => {
                            if (call.args.len != 0) return error.TypeMismatch;
                            const target_ast: Type = try .fromAst(alloc, io, &generic.arguments[0].type.payload, c, m);
                            defer target_ast.deinit(alloc);
                            const target_comp = try c.compileType(alloc, io, &target_ast, m, generic.arguments[0].type.pos);
                            defer alloc.free(target_comp);

                            return try std.fmt.allocPrint(alloc, "sizeof({s})", .{target_comp});
                        },
                        else => {},
                    }
                }
            }
            const lhs_t = try Type.infer(alloc, io, call.callee, c, m);
            defer lhs_t.deinit(alloc);

            if (lhs_t != .function) return errors.expressionNotCallable(io, lhs_t, m.source_map[call.pos]);

            const function_t = lhs_t.function;

            const is_variadic: ?usize = b: {
                for (lhs_t.function.parameters, 0..) |param, i|
                    if (param == .variadic) break :b i;
                break :b null;
            };

            if (is_variadic) |param_index| {
                if (call.args.len < param_index)
                    return errors.argumentCount(io, function_t.parameters.len, call.args.len, m.source_map[call.pos]);
            } else if (function_t.parameters.len != call.args.len)
                return errors.argumentCount(io, function_t.parameters.len, call.args.len, m.source_map[call.pos]);

            var buf: std.ArrayList(u8) = .empty;
            errdefer buf.deinit(alloc);

            const lhs = try compile(alloc, io, call.callee, c, m, .{});
            defer alloc.free(lhs);
            try buf.appendSlice(alloc, lhs);

            for (function_t.parameters[0 .. is_variadic orelse function_t.parameters.len], 0..) |expected, i| {
                const received: Type = try .infer(alloc, io, &call.args[i], c, m);
                defer received.deinit(alloc);

                if (expected != .variadic and !received.check(expected))
                    return errors.typeMismatch(io, expected, received, m.source_map[call.args[i].pos()]);
            }

            try buf.append(alloc, '(');
            for (call.args, 0..) |*arg, i| {
                const arg_comp = try compile(alloc, io, arg, c, m, .{
                    .expected_type = if (i < function_t.parameters.len) function_t.parameters[i] else null,
                });
                defer alloc.free(arg_comp);
                try buf.appendSlice(alloc, arg_comp);
                if (i < call.args.len - 1) try buf.append(alloc, ',');
            }
            try buf.append(alloc, ')');

            return try buf.toOwnedSlice(alloc);
        },
        .type => |t_ast| {
            const t: Type = try .fromAst(alloc, io, &t_ast.payload, c, m);
            defer t.deinit(alloc);

            return try c.compileType(alloc, io, &t, m, t_ast.pos);
        },
        .assignment => |assignment| {
            const expected_type: Type = try .infer(alloc, io, assignment.assignee, c, m);
            defer expected_type.deinit(alloc);

            if (!try c.module.getExpressionMutability(alloc, io, assignment.assignee, c))
                return errors.assignmentOnNonMut(io, m.source_map[assignment.pos]);

            const lhs = try compile(alloc, io, assignment.assignee, c, m, .{});
            defer alloc.free(lhs);

            const rhs = try compile(alloc, io, assignment.value, c, m, .{ .expected_type = expected_type });
            defer alloc.free(rhs);

            return if (assignment.op == .@"^=")
                // the line below will re-evaluate the lhs twice, but the check above for `getExpressionMutability`
                // ensures that only idempotent lhs can be assigned.
                try std.fmt.allocPrint(alloc, "{s} = pow({s}, {s});", .{ lhs, lhs, rhs })
            else
                try std.fmt.allocPrint(alloc, "{s} {s} {s};", .{ lhs, @tagName(assignment.op), rhs });
        },
        .reference => |ref| {
            const child = try compile(alloc, io, ref.inner, c, m, .{});
            defer alloc.free(child);
            return try std.fmt.allocPrint(alloc, "&{s}", .{child});
        },
        .dereference => |deref| {
            const parent = try compile(alloc, io, deref.parent, c, m, .{});
            defer alloc.free(parent);
            return try std.fmt.allocPrint(alloc, "*{s}", .{parent});
        },
        .member => |member| {
            var parent_t: Type = try .infer(alloc, io, member.parent, c, m);
            var is_ref = false;
            while (parent_t == .reference) {
                is_ref = true;
                const next_t = try parent_t.reference.inner.clone(alloc);
                parent_t.deinit(alloc);
                parent_t = next_t;
            }
            defer parent_t.deinit(alloc);

            const parent_comp = try compile(alloc, io, member.parent, c, m, .{});
            defer alloc.free(parent_comp);

            const op = if (is_ref) "->" else ".";

            return switch (parent_t) {
                inline .@"struct", .@"union", .@"enum" => |ct, tag| {
                    if (!ct.hasMember(member.member_name))
                        return errors.unknownMember(io, parent_t, member.member_name, m.source_map[member.pos]);

                    for (ct.members) |member_m| if (std.mem.eql(u8, member_m.name, member.member_name)) {
                        return if (tag == .@"enum")
                            try alloc.dupe(u8, member_m.inner_name)
                        else
                            try std.fmt.allocPrint(alloc, "{s}{s}{s}", .{ parent_comp, op, member_m.inner_name });
                    };

                    for (ct.symbols) |s| if (std.mem.eql(u8, s.name, member.member_name))
                        return try alloc.dupe(u8, s.inner_name);

                    unreachable;
                },
                .slice => if (std.mem.eql(u8, member.member_name, "ptr") or
                    std.mem.eql(u8, member.member_name, "len"))
                    std.fmt.allocPrint(alloc, "{s}.{s}", .{ parent_comp, member.member_name })
                else
                    errors.badMemberAccessSlice(io, parent_t, member.member_name, m.source_map[member.pos]),
                .module => |module| if (module.getSymbol(member.member_name)) |symbol|
                    try alloc.dupe(u8, symbol.inner_name)
                else
                    errors.unknownMember(io, parent_t, member.member_name, m.source_map[member.pos]),
                else => errors.badMemberAccess(io, parent_t, member.member_name, m.source_map[member.pos]),
            };
        },
        .prefix => |prefix| {
            const rhs_t: Type = try .infer(alloc, io, prefix.rhs, c, m);
            defer rhs_t.deinit(alloc);

            if (rhs_t == .bool and prefix.op != .@"!") return errors.badBangPrefix(io, rhs_t, m.source_map[prefix.pos]);
            if ((rhs_t.isNumeric() or rhs_t == .reference) and prefix.op != .@"-")
                return errors.badDashPrefix(io, rhs_t, m.source_map[prefix.pos]);

            const rhs_comp = try compile(alloc, io, prefix.rhs, c, m, .{});
            defer alloc.free(rhs_comp);

            return std.fmt.allocPrint(alloc, "{s}{s}", .{ @tagName(prefix.op), rhs_comp });
        },
        .index => |index| {
            const lhs_t: Type = try .infer(alloc, io, index.lhs, c, m);
            defer lhs_t.deinit(alloc);
            if (lhs_t != .slice and lhs_t != .array) return errors.illegalIndex(io, lhs_t, m.source_map[index.pos]);

            const index_t: Type = try .infer(alloc, io, index.index, c, m);
            defer index_t.deinit(alloc);
            if (!index_t.isInteger()) return errors.illegalIndexType(io, index_t, m.source_map[index.index.pos()]);

            const lhs_comp = try compile(alloc, io, index.lhs, c, m, .{});
            defer alloc.free(lhs_comp);

            const index_comp = try compile(alloc, io, index.index, c, m, .{});
            defer alloc.free(index_comp);

            return try std.fmt.allocPrint(alloc, "{s}{s}[{s}]", .{
                lhs_comp,
                if (lhs_t == .slice) ".ptr" else if (lhs_t == .array) ".items" else unreachable,
                index_comp,
            });
        },
        .binary => |binary| {
            var buf: std.ArrayList(u8) = .empty;
            errdefer buf.deinit(alloc);

            const lhs_t: Type = try .infer(alloc, io, binary.lhs, c, m);
            const rhs_t: Type = try .infer(alloc, io, binary.rhs, c, m);
            const expr_is_optional = (lhs_t == .optional and rhs_t == .@"typeof(nil)") or
                (lhs_t == .optional and rhs_t == .optional and lhs_t.optional.eql(rhs_t.optional.*)) or
                (rhs_t == .optional and lhs_t == .@"typeof(nil)");
            const expr_is_type = lhs_t == .type and rhs_t == .type;

            if (!((lhs_t.isNumeric() and rhs_t.isNumeric()) or
                (lhs_t == .bool and rhs_t == .bool) or
                (lhs_t == .reference and rhs_t == .reference) or
                (lhs_t == .@"enum" and rhs_t == .@"enum" and lhs_t.eql(rhs_t)) or
                expr_is_type or expr_is_optional))
                return errors.illegalBinaryExpression(io, lhs_t, binary.op, rhs_t, m.source_map[binary.pos]);

            switch (binary.op) {
                .@"^" => if (Value.eval(alloc, io, expr, c, m)) |comptime_expr| {
                    try buf.print(alloc, "{f}", .{comptime_expr});
                } else |_| {
                    const lhs_comp = try compile(alloc, io, binary.lhs, c, m, .{});
                    defer alloc.free(lhs_comp);

                    const rhs_comp = try compile(alloc, io, binary.rhs, c, m, .{});
                    defer alloc.free(rhs_comp);

                    try buf.print(alloc, "pow({s}, {s})", .{ lhs_comp, rhs_comp });
                },
                else => if (expr_is_optional) {
                    try buf.appendSlice(alloc, "({");

                    const lhs_t_comp = try c.compileType(alloc, io, &lhs_t, m, binary.lhs.pos());
                    defer alloc.free(lhs_t_comp);

                    var lhs_name: [17]u8 = undefined;
                    _ = try std.fmt.bufPrint(&lhs_name, "_{x}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(binary.lhs))});
                    const lhs_comp = try compile(alloc, io, binary.lhs, c, m, .{});
                    defer alloc.free(lhs_comp);

                    const rhs_t_comp = try c.compileType(alloc, io, &if (rhs_t == .@"typeof(nil)") lhs_t else rhs_t, m, binary.pos);
                    defer alloc.free(rhs_t_comp);
                    var rhs_name: [17]u8 = undefined;
                    _ = try std.fmt.bufPrint(&rhs_name, "_{x}", .{std.hash.Wyhash.hash(0, std.mem.asBytes(binary.rhs))});
                    const rhs_comp = try compile(alloc, io, binary.rhs, c, m, .{});
                    defer alloc.free(rhs_comp);

                    try buf.print(alloc, "{s} {s} = {s};", .{ lhs_t_comp, lhs_name, lhs_comp });
                    try buf.print(alloc, "{s} {s} = {s};", .{ rhs_t_comp, rhs_name, rhs_comp });

                    try buf.appendSlice(alloc, "((");
                    if (lhs_t == .@"typeof(nil)") {
                        try buf.appendSlice(alloc, "false");
                    } else try buf.print(alloc, "{s}.is_some", .{lhs_name});

                    try buf.appendSlice(alloc, " == ");

                    if (rhs_t == .@"typeof(nil)") {
                        try buf.appendSlice(alloc, "false");
                    } else try buf.print(alloc, "{s}.is_some", .{rhs_name});

                    try buf.print(
                        alloc,
                        ") && (!({[lhs]s}.is_some && {[rhs]s}.is_some) || ({[lhs]s}.payload == {[rhs]s}.payload))",
                        .{ .lhs = lhs_name, .rhs = rhs_name },
                    );

                    try buf.appendSlice(alloc, "})");
                } else if (expr_is_type) {
                    const lhs_symbol = c.module.getSymbolFromExpression(alloc, io, binary.lhs, c).?;
                    const rhs_symbol = c.module.getSymbolFromExpression(alloc, io, binary.rhs, c).?;
                    try buf.print(alloc, "{}", .{lhs_symbol.value.?.type.eql(rhs_symbol.value.?.type)});
                } else {
                    const lhs_comp = try compile(alloc, io, binary.lhs, c, m, .{});
                    defer alloc.free(lhs_comp);
                    const rhs_comp = try compile(alloc, io, binary.rhs, c, m, .{});
                    defer alloc.free(rhs_comp);

                    try buf.print(alloc, "{s} {s} {s}", .{ lhs_comp, switch (binary.op) {
                        .@"and", .but => "&&",
                        .@"or" => "||",
                        else => |op| @tagName(op),
                    }, rhs_comp });
                },
            }

            return try buf.toOwnedSlice(alloc);
        },
        .struct_instantiation => |si| {
            const t: Type = try .infer(alloc, io, si.type_expr, c, m);
            defer t.deinit(alloc);
            if (t != .type) return errors.exprIsNotStruct(io, t, m.source_map[si.pos]);

            const symbol = c.module.getSymbolFromExpression(alloc, io, si.type_expr, c) orelse
                return errors.exprIsNotStruct(io, t, m.source_map[si.pos]);

            if (symbol.value == null or symbol.value.? != .type or
                (symbol.value.?.type != .@"struct" and
                    symbol.value.?.type != .@"union"))
            {
                return errors.exprIsNotStruct(io, t, m.source_map[si.pos]);
            }

            const si_t = symbol.value.?.type;
            if (si_t == .@"struct") {
                // check for missing members
                var received: std.BufSet = .init(alloc);
                defer received.deinit();

                for (si.members) |member| try received.insert(member.name);

                var missing: std.ArrayList([]const u8) = .empty;
                defer missing.deinit(alloc);

                for (si_t.@"struct".members) |member_m| if (!received.contains(member_m.name))
                    try missing.append(alloc, member_m.name);

                if (missing.items.len > 0)
                    return errors.missingStructMembers(io, si_t, missing.items, m.source_map[si.pos]);

                // check for extraneous members
                var expected: std.BufSet = .init(alloc);
                defer expected.deinit();

                for (si_t.@"struct".members) |member_m| try expected.insert(member_m.name);

                var extraneous: std.ArrayList([]const u8) = .empty;
                defer extraneous.deinit(alloc);

                for (si.members) |member| if (!expected.contains(member.name))
                    try extraneous.append(alloc, member.name);

                if (extraneous.items.len > 0)
                    return errors.extraneousStructMembers(io, si_t, extraneous.items, m.source_map[si.pos]);
            } else if (si.members.len != 1) return errors.unionMemberCount(io, si_t, si.members.len, m.source_map[si.pos]);

            var ret: std.ArrayList(u8) = .empty;
            errdefer ret.deinit(alloc);

            const t_comp = try c.compileType(alloc, io, &si_t, m, si.type_expr.pos());
            defer alloc.free(t_comp);

            try ret.print(alloc, "({s}){{", .{t_comp});
            for (si.members) |member| {
                const expected = switch (si_t) {
                    inline .@"struct", .@"union" => |ct| ct.getMemberType(member.name).?,
                    else => unreachable,
                };
                const expr_comp = try compile(alloc, io, &member.value, c, m, .{
                    .expected_type = expected,
                });
                defer alloc.free(expr_comp);
                try ret.print(alloc, ".{s} = {s},", .{ member.name, expr_comp });
            }
            try ret.append(alloc, '}');

            return try ret.toOwnedSlice(alloc);
        },
        .comparison => |comparison| {
            var buf: std.ArrayList(u8) = .empty;

            try buf.appendSlice(alloc, "({");

            var final_eval: std.ArrayList(u8) = .empty;
            defer final_eval.deinit(alloc);

            var left = comparison.left;
            for (comparison.comparisons, 0..) |comp, i| {
                const lhs_t: Type = try .infer(alloc, io, left, c, m);
                defer lhs_t.deinit(alloc);

                const right = comp.right;

                const rhs_t: Type = try .infer(alloc, io, right, c, m);
                defer rhs_t.deinit(alloc);

                if (!lhs_t.eql(rhs_t))
                    return errors.typeMismatchBinExpr(io, lhs_t, rhs_t, comp.op, m.source_map[left.pos()]);

                if (lhs_t.isNumeric() and
                    (comp.op == .@"and" or comp.op == .@"or" or comp.op == .but))
                    return errors.booleanOperatorUsedOnNumerical(io, lhs_t, comp.op, m.source_map[left.pos()]);

                if (lhs_t == .bool and
                    (comp.op != .@"and" and comp.op != .@"or" and comp.op != .but))
                    return errors.numericalOperatorUsedOnBoolean(io, lhs_t, comp.op, m.source_map[left.pos()]);

                const t_comp = try c.compileType(alloc, io, &lhs_t, m, left.pos());
                defer alloc.free(t_comp);

                const left_comp = try compile(alloc, io, left, c, m, .{});
                defer alloc.free(left_comp);

                const right_comp = try compile(alloc, io, right, c, m, .{});
                defer alloc.free(right_comp);

                if (i == 0) {
                    const lhs_name = std.hash.Wyhash.hash(0, std.mem.asBytes(&lhs_t));
                    try buf.print(alloc, "{s} _{x} = {s};", .{ t_comp, lhs_name, left_comp });
                    try final_eval.print(alloc, "_{x} ", .{lhs_name});
                }

                const rhs_name = std.hash.Wyhash.hash(0, std.mem.asBytes(&rhs_t));
                try buf.print(alloc, "{s} _{x}{} = {s};", .{ t_comp, rhs_name, i, right_comp });
                try final_eval.print(alloc, "{s} _{x}{} && _{1x}{2} ", .{ @tagName(comp.op), rhs_name, i });

                left = right;
            }

            try buf.appendSlice(alloc, final_eval.items);
            try buf.appendSlice(alloc, ";})");
            return try buf.toOwnedSlice(alloc);
        },
        .slice => |slice| {
            const lhs_t: Type = try .infer(alloc, io, slice.lhs, c, m);
            defer lhs_t.deinit(alloc);

            if (lhs_t != .slice and lhs_t != .array) return errors.cannotSlice(io, lhs_t, m.source_map[slice.pos]);

            const lhs_comp = try compile(alloc, io, slice.lhs, c, m, .{});
            defer alloc.free(lhs_comp);

            const start_t: Type = try .infer(alloc, io, slice.start, c, m);
            defer start_t.deinit(alloc);
            if (!start_t.isInteger()) return errors.illegalIndexType(io, start_t, m.source_map[slice.start.pos()]);

            if (slice.end) |end| {
                const end_t: Type = try .infer(alloc, io, end, c, m);
                defer end_t.deinit(alloc);
                if (!end_t.isInteger()) return errors.illegalIndexType(io, end_t, m.source_map[slice.start.pos()]);
            }

            const start_comp = try compile(alloc, io, slice.start, c, m, .{});
            defer alloc.free(start_comp);

            const end_comp = if (slice.end) |end|
                try compile(alloc, io, end, c, m, .{})
            else if (lhs_t == .slice)
                try std.fmt.allocPrint(alloc, "{s}.ptr + {0s}.len", .{lhs_comp})
            else
                try std.fmt.allocPrint(alloc, "{s}.items + {}", .{ lhs_comp, lhs_t.array.len });
            defer alloc.free(end_comp);

            const t_comp = switch (lhs_t) {
                .slice => try c.compileType(alloc, io, &lhs_t, m, slice.pos),
                .array => try c.compileType(alloc, io, &.{
                    .slice = .{
                        .inner = lhs_t.array.inner,
                        .is_mut = try c.module.getExpressionMutability(alloc, io, slice.lhs, c),
                    },
                }, m, slice.pos),
                else => unreachable,
            };
            defer alloc.free(t_comp);

            return try std.fmt.allocPrint(alloc, "({s}){{ .ptr = {s}{s} + {s}, .len = (size_t)({s} - {3s}) }}", .{
                t_comp,
                lhs_comp,
                if (lhs_t == .slice) ".ptr" else ".items",
                start_comp,
                end_comp,
            });
        },
        .range => unreachable,
        .match => |match| {
            var buf: std.ArrayList(u8) = .empty;

            const condition_t: Type = try .infer(alloc, io, match.condition, c, m);
            defer condition_t.deinit(alloc);

            const condition_comp = try compile(alloc, io, match.condition, c, m, .{});
            defer alloc.free(condition_comp);

            try buf.print(alloc, "switch ({s}) {{", .{condition_comp});

            for (match.cases) |case| {
                switch (case.condition) {
                    .opts => |cases| for (cases) |*single| {
                        const single_comp = try compile(alloc, io, single, c, m, .{});
                        defer alloc.free(single_comp);
                        try buf.print(alloc, "case {s}:", .{single_comp});
                    },
                    .@"else" => try buf.appendSlice(alloc, "default:"),
                }

                const result_comp = try statements.compile(alloc, io, &case.result, c, m);
                defer alloc.free(result_comp);

                try buf.print(alloc, "{{ {s} }} break;", .{result_comp});
            }

            try buf.append(alloc, '}');

            return try buf.toOwnedSlice(alloc);
        },
        else => std.debug.panic("{}", .{expr.*}),
    };
}

const std = @import("std");
const ast = @import("ast");

const errors = @import("errors.zig");
const statements = @import("statements.zig");
const compiler = @import("compiler.zig");
const Compiler = compiler.Compiler;
const Type = compiler.Type;
const Value = compiler.Value;
const Error = errors.Error;

pub fn compile(
    alloc: std.mem.Allocator,
    io: std.Io,
    expr: *const ast.Expression,
    c: *Compiler,
    opts: struct {
        expected_type: ?Type = null,
    },
) Error![]const u8 {
    if (opts.expected_type) |et| {
        const received: Type = try .infer(alloc, io, expr, c);
        defer received.deinit(alloc);

        if (et.eql(received)) return compile(alloc, io, expr, c, .{});
        if (!received.check(et)) return errors.typeMismatch(io, et, received, c.source_map[expr.pos()]);

        switch (et) {
            .optional => {
                const expected_comp = try c.compileType(alloc, io, &et, expr.pos());
                defer alloc.free(expected_comp);

                const base = try compile(alloc, io, expr, c, .{});
                defer alloc.free(base);
                return std.fmt.allocPrint(alloc, "({s}){{ .is_some = true, .payload = {s} }}", .{
                    expected_comp,
                    base,
                });
            },
            else => return compile(alloc, io, expr, c, .{}),
        }
    }

    return switch (expr.*) {
        .bad_node => unreachable,
        .ident => |ident| if (c.module.getSymbol(ident.payload)) |symbol|
            try alloc.dupe(u8, symbol.inner_name)
        else
            errors.unknownSymbol(io, ident.payload, c.source_map[ident.pos]),
        .string => |string| try std.fmt.allocPrint(alloc, "\"{s}\"", .{string.payload}),
        .char => |char| try std.fmt.allocPrint(alloc, "'{c}'", .{char.payload}),
        inline .int, .float => |number| try std.fmt.allocPrint(alloc, "{}", .{number.payload}),
        .@"if" => |cond| {
            const condition = try compile(alloc, io, cond.condition, c, .{ .expected_type = .bool });
            defer alloc.free(condition);
            const a = try compile(alloc, io, cond.body, c, .{});
            defer alloc.free(a);
            const b = try compile(alloc, io, cond.@"else", c, .{});
            defer alloc.free(b);
            return std.fmt.allocPrint(alloc, "({s}) ? {s} : {s}", .{ condition, a, b });
        },
        .block => |block| {
            const block_comp = try statements.block(alloc, io, block.payload, c, .{});
            defer alloc.free(block_comp);
            return try std.fmt.allocPrint(alloc, "({{{s}}})", .{block_comp});
        },
        .generic => |generic| {
            const t: Type = try .infer(alloc, io, expr, c);
            defer t.deinit(alloc);

            // This is a bit hacky, but we can reconstruct the mangled name
            // or just use Type.instantiate to get the type and then find the symbol.
            // Since we know it's a function and it's in the module.

            const lhs_t: Type = try .infer(alloc, io, generic.lhs, c);
            defer lhs_t.deinit(alloc);

            const template_name = switch (lhs_t.template) {
                inline else => |d| d.name,
            };

            const mangled_name = try Type.getMangledName(alloc, io, template_name, generic.arguments, c);
            defer alloc.free(mangled_name);

            const symbol = c.module.getSymbol(mangled_name) orelse return error.InstantiationFailed;
            return try alloc.dupe(u8, symbol.inner_name);
        },
        .array_instantiation => |inst| {
            const inner_ast: Type = try .fromAst(alloc, io, &inst.type, c);
            defer inner_ast.deinit(alloc);

            var buf: std.ArrayList(u8) = .empty;
            errdefer buf.deinit(alloc);

            const inner_t = try c.compileType(alloc, io, &inner_ast, inst.type.pos());
            defer alloc.free(inner_t);

            const len: Value = if (inst.length.* == .ident and std.mem.eql(u8, inst.length.ident.payload, "_"))
                .{ .uint = inst.contents.len }
            else
                try .eval(alloc, io, inst.length, c);

            if (len != .uint)
                return errors.arrayLengthMustBeInteger(io, len.getType(), c.source_map[inst.length.pos()]);

            if (len.uint != inst.contents.len)
                return errors.arrayInstantiationSizeMismatch(io, len.uint, inst.contents.len, c.source_map[inst.length.pos()]);

            const t: Type = .{ .array = .{ .inner = &inner_ast, .len = len.uint } };
            const t_comp = try c.compileType(alloc, io, &t, inst.pos);
            defer alloc.free(t_comp);

            try buf.print(alloc, "({s})", .{t_comp});

            try buf.append(alloc, '{');
            for (inst.contents, 0..) |*item, i| {
                const item_comp = try compile(alloc, io, item, c, .{});
                defer alloc.free(item_comp);

                try buf.appendSlice(alloc, item_comp);
                try buf.append(alloc, if (i == inst.contents.len - 1) '}' else ',');
            }

            return try buf.toOwnedSlice(alloc);
        },
        .call => |call| {
            const lhs_t: Type = try .infer(alloc, io, call.callee, c);
            defer lhs_t.deinit(alloc);
            if (lhs_t != .function) return errors.expressionNotCallable(io, lhs_t, c.source_map[call.pos]);

            const function_t = lhs_t.function;

            const is_variadic: ?usize = b: {
                for (lhs_t.function.parameters, 0..) |param, i|
                    if (param == .variadic) break :b i;
                break :b null;
            };

            if (is_variadic) |param_index| {
                if (call.args.len < param_index)
                    return errors.argumentCount(io, function_t.parameters.len, call.args.len, c.source_map[call.pos]);
            } else if (function_t.parameters.len != call.args.len)
                return errors.argumentCount(io, function_t.parameters.len, call.args.len, c.source_map[call.pos]);

            var buf: std.ArrayList(u8) = .empty;
            errdefer buf.deinit(alloc);

            const lhs = try compile(alloc, io, call.callee, c, .{});
            defer alloc.free(lhs);
            try buf.appendSlice(alloc, lhs);

            for (function_t.parameters[0 .. is_variadic orelse function_t.parameters.len], 0..) |expected, i| {
                const received: Type = try .infer(alloc, io, &call.args[i], c);
                defer received.deinit(alloc);

                if (expected != .variadic and !received.check(expected))
                    return errors.typeMismatch(io, expected, received, c.source_map[call.args[i].pos()]);
            }

            try buf.append(alloc, '(');
            for (call.args, 0..) |*arg, i| {
                const arg_comp = try compile(alloc, io, arg, c, .{
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
            const t: Type = try .fromAst(alloc, io, &t_ast.payload, c);
            defer t.deinit(alloc);

            return try c.compileType(alloc, io, &t, t_ast.pos);
        },
        .assignment => |assignment| {
            const expected_type: Type = try .infer(alloc, io, assignment.assignee, c);
            defer expected_type.deinit(alloc);

            if (!try c.module.getExpressionMutability(alloc, io, assignment.assignee, c))
                return errors.assignmentOnNonMut(io, c.source_map[assignment.pos]);

            const lhs = try compile(alloc, io, assignment.assignee, c, .{});
            defer alloc.free(lhs);

            const rhs = try compile(alloc, io, assignment.value, c, .{ .expected_type = expected_type });
            defer alloc.free(rhs);

            return if (assignment.op == .@"^=")
                // the line below will re-evaluate the lhs twice, but the check above for `getExpressionMutability`
                // ensures that only idempotent lhs can be assigned.
                try std.fmt.allocPrint(alloc, "{s} = pow({s}, {s});", .{ lhs, lhs, rhs })
            else
                try std.fmt.allocPrint(alloc, "{s} {s} {s};", .{ lhs, @tagName(assignment.op), rhs });
        },
        .reference => |ref| {
            const child = try compile(alloc, io, ref.inner, c, .{});
            defer alloc.free(child);
            return try std.fmt.allocPrint(alloc, "&{s}", .{child});
        },
        .dereference => |deref| {
            const parent = try compile(alloc, io, deref.parent, c, .{});
            defer alloc.free(parent);
            return try std.fmt.allocPrint(alloc, "*{s}", .{parent});
        },
        .member => |member| {
            var parent_t: Type = try .infer(alloc, io, member.parent, c);
            var is_ref = false;
            while (parent_t == .reference) {
                is_ref = true;
                const next_t = try parent_t.reference.inner.clone(alloc);
                parent_t.deinit(alloc);
                parent_t = next_t;
            }
            defer parent_t.deinit(alloc);

            const parent_comp = try compile(alloc, io, member.parent, c, .{});
            defer alloc.free(parent_comp);

            const op = if (is_ref) "->" else ".";

            return switch (parent_t) {
                inline .@"struct", .@"union", .@"enum" => |ct, tag| {
                    if (!ct.hasMember(member.member_name))
                        return errors.unknownMember(io, parent_t, member.member_name, c.source_map[member.pos]);

                    for (ct.members) |m| if (std.mem.eql(u8, m.name, member.member_name)) {
                        return if (tag == .@"enum")
                            try alloc.dupe(u8, m.inner_name)
                        else
                            try std.fmt.allocPrint(alloc, "{s}{s}{s}", .{ parent_comp, op, m.inner_name });
                    };

                    for (ct.symbols) |s| if (std.mem.eql(u8, s.name, member.member_name))
                        return try alloc.dupe(u8, s.inner_name);

                    unreachable;
                },
                .slice => if (std.mem.eql(u8, member.member_name, "ptr") or
                    std.mem.eql(u8, member.member_name, "len"))
                    std.fmt.allocPrint(alloc, "{s}.{s}", .{ parent_comp, member.member_name })
                else
                    errors.badMemberAccessSlice(io, parent_t, member.member_name, c.source_map[member.pos]),
                .module => |module| if (module.getSymbol(member.member_name)) |symbol|
                    try alloc.dupe(u8, symbol.inner_name)
                else
                    errors.unknownMember(io, parent_t, member.member_name, c.source_map[member.pos]),
                else => errors.badMemberAccess(io, parent_t, member.member_name, c.source_map[member.pos]),
            };
        },
        .prefix => |prefix| {
            const rhs_t: Type = try .infer(alloc, io, prefix.rhs, c);
            defer rhs_t.deinit(alloc);

            if (rhs_t == .bool and prefix.op != .@"!") return errors.badBangPrefix(io, rhs_t, c.source_map[prefix.pos]);
            if ((rhs_t.isNumeric() or rhs_t == .reference) and prefix.op != .@"-")
                return errors.badDashPrefix(io, rhs_t, c.source_map[prefix.pos]);

            const rhs_comp = try compile(alloc, io, prefix.rhs, c, .{});
            defer alloc.free(rhs_comp);

            return std.fmt.allocPrint(alloc, "{s}{s}", .{ @tagName(prefix.op), rhs_comp });
        },
        .index => |index| {
            const lhs_t: Type = try .infer(alloc, io, index.lhs, c);
            defer lhs_t.deinit(alloc);
            if (lhs_t != .slice and lhs_t != .array) return errors.illegalIndex(io, lhs_t, c.source_map[index.pos]);

            const index_t: Type = try .infer(alloc, io, index.index, c);
            defer index_t.deinit(alloc);
            if (!index_t.isInteger()) return errors.illegalIndexType(io, index_t, c.source_map[index.index.pos()]);

            const lhs_comp = try compile(alloc, io, index.lhs, c, .{});
            defer alloc.free(lhs_comp);

            const index_comp = try compile(alloc, io, index.index, c, .{});
            defer alloc.free(index_comp);

            return try std.fmt.allocPrint(alloc, "{s}{s}[{s}]", .{
                lhs_comp,
                if (lhs_t == .slice) ".ptr" else if (lhs_t == .array) ".items" else unreachable,
                index_comp,
            });
        },
        .binary => |binary| {
            const lhs_t: Type = try .infer(alloc, io, binary.lhs, c);
            defer lhs_t.deinit(alloc);

            const rhs_t: Type = try .infer(alloc, io, binary.rhs, c);
            defer rhs_t.deinit(alloc);

            if (!lhs_t.eql(rhs_t))
                return errors.typeMismatchBinExpr(io, lhs_t, rhs_t, binary.op, c.source_map[binary.pos]);

            if (lhs_t.isNumeric() and
                (binary.op == .@"and" or binary.op == .@"or" or binary.op == .but))
                return errors.booleanOperatorUsedOnNumerical(io, lhs_t, binary.op, c.source_map[binary.pos]);

            if (lhs_t == .bool and
                (binary.op != .@"and" and binary.op != .@"or" and binary.op != .but))
                return errors.numericalOperatorUsedOnBoolean(io, lhs_t, binary.op, c.source_map[binary.pos]);

            const lhs_comp = try compile(alloc, io, binary.lhs, c, .{});
            defer alloc.free(lhs_comp);

            const rhs_comp = try compile(alloc, io, binary.rhs, c, .{});
            defer alloc.free(rhs_comp);

            return if (binary.op == .@"^")
                try std.fmt.allocPrint(alloc, "pow({s}, {s})", .{ lhs_comp, rhs_comp })
            else
                try std.fmt.allocPrint(alloc, "{s} {s} {s}", .{ lhs_comp, @tagName(binary.op), rhs_comp });
        },
        .struct_instantiation => |si| {
            const t: Type = try .infer(alloc, io, si.type_expr, c);
            defer t.deinit(alloc);
            if (t != .type) return errors.exprIsNotStruct(io, t, c.source_map[si.pos]);

            const symbol = c.module.getSymbolFromExpression(alloc, io, si.type_expr, c) orelse
                return errors.exprIsNotStruct(io, t, c.source_map[si.pos]);

            if (symbol.value == null or symbol.value.? != .type or
                (symbol.value.?.type != .@"struct" and
                    symbol.value.?.type != .@"union"))
            {
                return errors.exprIsNotStruct(io, t, c.source_map[si.pos]);
            }

            const si_t = symbol.value.?.type;
            if (si_t == .@"struct") {
                // check for missing members
                var received: std.BufSet = .init(alloc);
                defer received.deinit();

                for (si.members) |m| try received.insert(m.name);

                var missing: std.ArrayList([]const u8) = .empty;
                defer missing.deinit(alloc);

                for (si_t.@"struct".members) |m| if (!received.contains(m.name))
                    try missing.append(alloc, m.name);

                if (missing.items.len > 0)
                    return errors.missingStructMembers(io, si_t, missing.items, c.source_map[si.pos]);

                // check for extraneous members
                var expected: std.BufSet = .init(alloc);
                defer expected.deinit();

                for (si_t.@"struct".members) |m| try expected.insert(m.name);

                var extraneous: std.ArrayList([]const u8) = .empty;
                defer extraneous.deinit(alloc);

                for (si.members) |m| if (!expected.contains(m.name))
                    try extraneous.append(alloc, m.name);

                if (extraneous.items.len > 0)
                    return errors.extraneousStructMembers(io, si_t, extraneous.items, c.source_map[si.pos]);
            } else if (si.members.len != 1) return errors.unionMemberCount(io, si_t, si.members.len, c.source_map[si.pos]);

            var ret: std.ArrayList(u8) = .empty;
            errdefer ret.deinit(alloc);

            const t_comp = try c.compileType(alloc, io, &si_t, si.type_expr.pos());
            defer alloc.free(t_comp);

            try ret.print(alloc, "({s}){{", .{t_comp});
            for (si.members) |member| {
                const expected = switch (si_t) {
                    inline .@"struct", .@"union" => |ct| ct.getMemberType(member.name).?,
                    else => unreachable,
                };
                const expr_comp = try compile(alloc, io, &member.value, c, .{
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
                const lhs_t: Type = try .infer(alloc, io, left, c);
                defer lhs_t.deinit(alloc);

                const right = comp.right;

                const rhs_t: Type = try .infer(alloc, io, right, c);
                defer rhs_t.deinit(alloc);

                if (!lhs_t.eql(rhs_t))
                    return errors.typeMismatchBinExpr(io, lhs_t, rhs_t, comp.op, c.source_map[left.pos()]);

                if (lhs_t.isNumeric() and
                    (comp.op == .@"and" or comp.op == .@"or" or comp.op == .but))
                    return errors.booleanOperatorUsedOnNumerical(io, lhs_t, comp.op, c.source_map[left.pos()]);

                if (lhs_t == .bool and
                    (comp.op != .@"and" and comp.op != .@"or" and comp.op != .but))
                    return errors.numericalOperatorUsedOnBoolean(io, lhs_t, comp.op, c.source_map[left.pos()]);

                const t_comp = try c.compileType(alloc, io, &lhs_t, left.pos());
                defer alloc.free(t_comp);

                const left_comp = try compile(alloc, io, left, c, .{});
                defer alloc.free(left_comp);

                const right_comp = try compile(alloc, io, right, c, .{});
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
            const lhs_t: Type = try .infer(alloc, io, slice.lhs, c);
            defer lhs_t.deinit(alloc);

            if (lhs_t != .slice and lhs_t != .array) return errors.cannotSlice(io, lhs_t, c.source_map[slice.pos]);

            const lhs_comp = try compile(alloc, io, slice.lhs, c, .{});
            defer alloc.free(lhs_comp);

            const start_t: Type = try .infer(alloc, io, slice.start, c);
            defer start_t.deinit(alloc);
            if (!start_t.isInteger()) return errors.illegalIndexType(io, start_t, c.source_map[slice.start.pos()]);

            if (slice.end) |end| {
                const end_t: Type = try .infer(alloc, io, end, c);
                defer end_t.deinit(alloc);
                if (!end_t.isInteger()) return errors.illegalIndexType(io, end_t, c.source_map[slice.start.pos()]);
            }

            const start_comp = try compile(alloc, io, slice.start, c, .{});
            defer alloc.free(start_comp);

            const end_comp = if (slice.end) |end|
                try compile(alloc, io, end, c, .{})
            else if (lhs_t == .slice)
                try std.fmt.allocPrint(alloc, "{s}.ptr + {0s}.len", .{lhs_comp})
            else
                try std.fmt.allocPrint(alloc, "{s}.items + {}", .{ lhs_comp, lhs_t.array.len });
            defer alloc.free(end_comp);

            const t_comp = switch (lhs_t) {
                .slice => try c.compileType(alloc, io, &lhs_t, slice.pos),
                .array => try c.compileType(alloc, io, &.{
                    .slice = .{
                        .inner = lhs_t.array.inner,
                        .is_mut = try c.module.getExpressionMutability(alloc, io, slice.lhs, c),
                    },
                }, slice.pos),
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

            const condition_t: Type = try .infer(alloc, io, match.condition, c);
            defer condition_t.deinit(alloc);

            const condition_comp = try compile(alloc, io, match.condition, c, .{});
            defer alloc.free(condition_comp);

            try buf.print(alloc, "switch ({s}) {{", .{condition_comp});

            for (match.cases) |case| {
                switch (case.condition) {
                    .opts => |cases| for (cases) |*single| {
                        const single_comp = try compile(alloc, io, single, c, .{});
                        defer alloc.free(single_comp);
                        try buf.print(alloc, "case {s}:", .{single_comp});
                    },
                    .@"else" => try buf.appendSlice(alloc, "default:"),
                }

                const result_comp = try statements.compile(alloc, io, &case.result, c);
                defer alloc.free(result_comp);

                try buf.print(alloc, "{{ {s} }} break;", .{result_comp});
            }

            try buf.append(alloc, '}');

            return try buf.toOwnedSlice(alloc);
        },
        else => std.debug.panic("{}", .{expr.*}),
    };
}

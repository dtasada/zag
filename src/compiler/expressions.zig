const std = @import("std");
const ast = @import("ast");

const errors = @import("errors.zig");
const statements = @import("statements.zig");
const compiler = @import("compiler.zig");
const Compiler = compiler.Compiler;
const Type = compiler.Type;
const Value = compiler.Value;

pub fn compile(
    alloc: std.mem.Allocator,
    expr: *const ast.Expression,
    c: *Compiler,
    opts: struct {
        expected_type: ?Type = null,
        is_variable_decl: bool = false,
    },
) ![]const u8 {
    return switch (expr.*) {
        .bad_node => unreachable,
        .ident => |ident| if (c.module.getSymbol(ident.payload)) |symbol|
            try alloc.dupe(u8, symbol.inner_name)
        else
            errors.unknownSymbol(ident.payload, c.source_map[ident.pos]),
        .string => |string| try std.fmt.allocPrint(alloc, "\"{s}\"", .{string.payload}),
        .char => |char| try std.fmt.allocPrint(alloc, "'{c}'", .{char.payload}),
        inline .int, .float => |number| try std.fmt.allocPrint(alloc, "{}", .{number.payload}),
        .@"if" => |cond| {
            const condition = try compile(alloc, cond.condition, c, .{ .expected_type = .bool });
            defer alloc.free(condition);
            const a = try compile(alloc, cond.body, c, .{});
            defer alloc.free(a);
            const b = try compile(alloc, cond.@"else", c, .{});
            defer alloc.free(b);
            return std.fmt.allocPrint(alloc, "({s}) ? {s} : {s}", .{ condition, a, b });
        },
        .block => |block| {
            const block_comp = try statements.block(alloc, block.payload, c);
            defer alloc.free(block_comp);
            return try std.fmt.allocPrint(alloc, "({{{s}}})", .{block_comp});
        },
        .array_instantiation => |inst| {
            const inner_ast: Type = try .fromAst(alloc, &inst.type, c);
            defer inner_ast.deinit(alloc);

            var buf: std.ArrayList(u8) = .empty;
            errdefer buf.deinit(alloc);

            if (!opts.is_variable_decl) {
                const inner_t = try c.compileType(alloc, &inner_ast, inst.type.pos());
                defer alloc.free(inner_t);

                const len: Value = try .eval(inst.length, c);
                if (len != .uint)
                    return errors.arrayLengthMustBeInteger(len.getType(), c.source_map[inst.length.pos()]);

                try buf.print(alloc, "({s}[{}])", .{ inner_t, len.uint });
            }

            try buf.append(alloc, '{');
            for (inst.contents, 0..) |*item, i| {
                const item_comp = try compile(alloc, item, c, .{});
                defer alloc.free(item_comp);

                try buf.appendSlice(alloc, item_comp);
                try buf.append(alloc, if (i == inst.contents.len - 1) '}' else ',');
            }

            return try buf.toOwnedSlice(alloc);
        },
        .call => |call| {
            const lhs_t: Type = try .infer(alloc, call.callee, c);
            defer lhs_t.deinit(alloc);
            if (lhs_t != .function) return errors.expressionNotCallable(lhs_t, c.source_map[call.pos]);

            const function_t = lhs_t.function;

            const is_variadic: ?usize = b: {
                for (lhs_t.function.parameters, 0..) |param, i|
                    if (param == .variadic) break :b i;
                break :b null;
            };

            if (is_variadic) |param_index| {
                if (call.args.len < param_index)
                    return errors.argumentCount(function_t.parameters.len, call.args.len, c.source_map[call.pos]);
            } else if (function_t.parameters.len != call.args.len)
                return errors.argumentCount(function_t.parameters.len, call.args.len, c.source_map[call.pos]);

            var buf: std.ArrayList(u8) = .empty;
            errdefer buf.deinit(alloc);

            const lhs = try compile(alloc, call.callee, c, .{});
            defer alloc.free(lhs);
            try buf.appendSlice(alloc, lhs);

            for (function_t.parameters[0 .. is_variadic orelse function_t.parameters.len], 0..) |expected, i| {
                const received: Type = try .infer(alloc, &call.args[i], c);
                defer received.deinit(alloc);

                if (expected != .variadic and !received.check(expected))
                    return errors.typeMismatch(expected, received, c.source_map[call.args[i].pos()]);
            }

            try buf.append(alloc, '(');
            for (call.args, 0..) |*arg, i| {
                const arg_comp = try compile(alloc, arg, c, .{
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
            const t: Type = try .fromAst(alloc, &t_ast.payload, c);
            defer t.deinit(alloc);

            return try c.compileType(alloc, &t, t_ast.pos);
        },
        .assignment => |assignment| {
            const expected_type: Type = try .infer(alloc, assignment.assignee, c);
            defer expected_type.deinit(alloc);

            if (!try c.module.getExpressionMutability(alloc, assignment.assignee, c))
                return errors.assignmentOnNonMut(c.source_map[assignment.pos]);

            const lhs = try compile(alloc, assignment.assignee, c, .{});
            defer alloc.free(lhs);

            const rhs = try compile(alloc, assignment.value, c, .{ .expected_type = expected_type });
            defer alloc.free(rhs);

            return if (assignment.op == .@"^=")
                // the line below will re-evaluate the lhs twice, but the check above for `getExpressionMutability`
                // ensures that only idempotent lhs can be assigned.
                try std.fmt.allocPrint(alloc, "{s} = pow({s}, {s});", .{ lhs, lhs, rhs })
            else
                try std.fmt.allocPrint(alloc, "{s} {s} {s}];", .{ lhs, @tagName(assignment.op), rhs });
        },
        .reference => |ref| {
            const child = try compile(alloc, ref.inner, c, .{});
            defer alloc.free(child);
            return try std.fmt.allocPrint(alloc, "&{s}", .{child});
        },
        .dereference => |deref| {
            const parent = try compile(alloc, deref.parent, c, .{});
            defer alloc.free(parent);
            return try std.fmt.allocPrint(alloc, "*{s}", .{parent});
        },
        .member => |member| {
            const parent_t: Type = try .infer(alloc, member.parent, c);
            defer parent_t.deinit(alloc);

            const parent_comp = try compile(alloc, member.parent, c, .{});
            defer alloc.free(parent_comp);

            return switch (parent_t) {
                inline .@"struct", .@"union", .@"enum" => |ct, tag| {
                    if (!ct.hasMember(member.member_name))
                        return errors.unknownMember(parent_t, member.member_name, c.source_map[member.pos]);

                    for (ct.members) |m| if (std.mem.eql(u8, m.name, member.member_name)) {
                        return if (tag == .@"enum")
                            try alloc.dupe(u8, m.inner_name)
                        else
                            try std.fmt.allocPrint(alloc, "{s}.{s}", .{ parent_comp, m.inner_name });
                    };

                    for (ct.symbols) |s| if (std.mem.eql(u8, s.name, member.member_name))
                        return try alloc.dupe(u8, s.inner_name);

                    unreachable;
                },
                .slice => if (std.mem.eql(u8, member.member_name, "ptr") or
                    std.mem.eql(u8, member.member_name, "len"))
                    std.fmt.allocPrint(alloc, "{s}.{s}", .{ parent_comp, member.member_name })
                else
                    errors.badMemberAccessSlice(parent_t, member.member_name, c.source_map[member.pos]),
                else => errors.badMemberAccess(parent_t, member.member_name, c.source_map[member.pos]),
            };
        },
        .prefix => |prefix| {
            const rhs_t: Type = try .infer(alloc, prefix.rhs, c);
            defer rhs_t.deinit(alloc);

            if (rhs_t == .bool and prefix.op != .@"!") return errors.badBangPrefix(rhs_t, c.source_map[prefix.pos]);
            if ((rhs_t.isNumeric() or rhs_t == .reference) and prefix.op != .@"-")
                return errors.badDashPrefix(rhs_t, c.source_map[prefix.pos]);

            const rhs_comp = try compile(alloc, prefix.rhs, c, .{});
            defer alloc.free(rhs_comp);

            return std.fmt.allocPrint(alloc, "{s}{s}", .{ @tagName(prefix.op), rhs_comp });
        },
        .index => |index| {
            const lhs_t: Type = try .infer(alloc, index.lhs, c);
            defer lhs_t.deinit(alloc);
            if (lhs_t != .slice and lhs_t != .array) return errors.illegalIndex(lhs_t, c.source_map[index.pos]);

            const index_t: Type = try .infer(alloc, index.index, c);
            defer index_t.deinit(alloc);
            if (!index_t.isInteger()) return errors.illegalIndexType(index_t, c.source_map[index.index.pos()]);

            const lhs_comp = try compile(alloc, index.lhs, c, .{});
            defer alloc.free(lhs_comp);

            const index_comp = try compile(alloc, index.index, c, .{});
            defer alloc.free(index_comp);

            return try std.fmt.allocPrint(alloc, "{s}{s}[{s}]", .{
                lhs_comp,
                if (index_t == .slice) ".ptr" else "",
                index_comp,
            });
        },
        .binary => |binary| {
            const lhs_t: Type = try .infer(alloc, binary.lhs, c);
            defer lhs_t.deinit(alloc);

            const rhs_t: Type = try .infer(alloc, binary.rhs, c);
            defer rhs_t.deinit(alloc);

            if (!lhs_t.eql(rhs_t))
                return errors.typeMismatchBinExpr(lhs_t, rhs_t, binary.op, c.source_map[binary.pos]);

            if (lhs_t.isNumeric() and
                (binary.op == .@"and" or binary.op == .@"or" or binary.op == .but))
                return errors.booleanOperatorUsedOnNumerical(lhs_t, binary.op, c.source_map[binary.pos]);

            if (lhs_t == .bool and
                (binary.op != .@"and" and binary.op != .@"or" and binary.op != .but))
                return errors.numericalOperatorUsedOnBoolean(lhs_t, binary.op, c.source_map[binary.pos]);

            const lhs_comp = try compile(alloc, binary.lhs, c, .{});
            defer alloc.free(lhs_comp);

            const rhs_comp = try compile(alloc, binary.rhs, c, .{});
            defer alloc.free(rhs_comp);

            return if (binary.op == .@"^")
                try std.fmt.allocPrint(alloc, "pow({s}, {s})", .{ lhs_comp, rhs_comp })
            else
                try std.fmt.allocPrint(alloc, "{s} {s} {s}", .{ lhs_comp, @tagName(binary.op), rhs_comp });
        },
        .struct_instantiation => |si| {
            const t: Type = try .infer(alloc, si.type_expr, c);
            defer t.deinit(alloc);
            if (t != .type) return errors.exprIsNotStruct(t, c.source_map[si.pos]);

            const result = c.module.getSymbolFromExpression(si.type_expr) orelse
                return errors.exprIsNotStruct(t, c.source_map[si.pos]);

            const symbol = if (result == .success)
                result.success
            else
                return errors.unknownSymbol(result.failure, c.source_map[si.type_expr.pos()]);

            if (symbol.value == null or symbol.value.? != .type or
                (symbol.value.?.type != .@"struct" and
                    symbol.value.?.type != .@"union"))
            {
                std.debug.print("symbol.value: {any}\n", .{symbol.value});
                return errors.exprIsNotStruct(t, c.source_map[si.pos]);
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
                    return errors.missingStructMembers(si_t, missing.items, c.source_map[si.pos]);

                // check for extraneous members
                var expected: std.BufSet = .init(alloc);
                defer expected.deinit();

                for (si_t.@"struct".members) |m| try expected.insert(m.name);

                var extraneous: std.ArrayList([]const u8) = .empty;
                defer extraneous.deinit(alloc);

                for (si.members) |m| if (!expected.contains(m.name))
                    try extraneous.append(alloc, m.name);

                if (extraneous.items.len > 0)
                    return errors.extraneousStructMembers(si_t, extraneous.items, c.source_map[si.pos]);
            } else if (si.members.len != 1) return errors.unionMemberCount(si_t, si.members.len, c.source_map[si.pos]);

            var ret: std.ArrayList(u8) = .empty;
            errdefer ret.deinit(alloc);

            const t_comp = try c.compileType(alloc, &si_t, si.type_expr.pos());
            defer alloc.free(t_comp);

            try ret.print(alloc, "({s}){{", .{t_comp});
            for (si.members) |member| {
                const expected = switch (si_t) {
                    inline .@"struct", .@"union" => |ct| ct.getMemberType(member.name).?,
                    else => unreachable,
                };
                const expr_comp = try compile(alloc, &member.value, c, .{
                    .is_variable_decl = true,
                    .expected_type = expected,
                });
                defer alloc.free(expr_comp);
                try ret.print(alloc, ".{s} = {s},", .{ member.name, expr_comp });
            }
            try ret.append(alloc, '}');

            return try ret.toOwnedSlice(alloc);
        },
        else => std.debug.panic("{}", .{expr.*}),
    };
}

pub fn @"return"(alloc: std.mem.Allocator, ret: ast.Statement.Return, c: *Compiler) ![]const u8 {
    _ = alloc;
    _ = ret;
    _ = c;
    unreachable;
}

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
            return try std.fmt.allocPrint(alloc, "({s})", .{block_comp});
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

            try buf.append(alloc, '(');
            for (call.args, 0..) |*arg, i| {
                const received: Type = try .infer(alloc, arg, c);
                defer received.deinit(alloc);
                const expected = function_t.parameters[i];

                if (expected != .variadic and !received.check(expected))
                    return errors.typeMismatch(expected, received, c.source_map[arg.pos()]);

                const arg_comp = try compile(alloc, arg, c, .{ .expected_type = expected });
                defer alloc.free(arg_comp);
                try buf.appendSlice(alloc, arg_comp);
                if (i < call.args.len - 1) try buf.append(alloc, ',');
            }
            try buf.append(alloc, ')');

            return try buf.toOwnedSlice(alloc);
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

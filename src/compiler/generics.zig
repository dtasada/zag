const std = @import("std");
const ast = @import("ast");

pub fn mapGenerics(
    alloc: std.mem.Allocator,
    map: std.StringHashMap(ast.Expression),
    node: anytype,
) error{ OutOfMemory, UnhandledGenericMappingTypeSubstitution }!void {
    const PtrT = @TypeOf(node);
    const info = @typeInfo(PtrT);
    if (info != .pointer) return;
    const T = info.pointer.child;
    const t_info = @typeInfo(T);

    if (T == []const u8 or
        T == usize or
        T == bool or
        T == f64 or
        T == ast.BinaryOperator or
        T == ast.AssignmentOperator or
        T == ast.PrefixOperator) return;

    if (T == ast.Expression and node.* == .ident)
        if (map.get(node.ident.payload)) |expr| {
            const mutable = @constCast(node);
            mutable.deinit(alloc);
            mutable.* = try expr.clone(alloc);
            return;
        };

    if (T == ast.Type and node.* == .symbol) {
        if (map.get(node.symbol.inner)) |expr| {
            const mutable = @constCast(node);
            if (expr == .ident) {
                const old_pos = node.symbol.pos;
                mutable.deinit(alloc);
                mutable.* = .{ .symbol = .{ .pos = old_pos, .inner = try alloc.dupe(u8, expr.ident.payload) } };
                return;
            } else if (expr == .type) {
                mutable.deinit(alloc);
                mutable.* = try expr.type.payload.clone(alloc);
                // We might need to adjust the position here if we care, but clone keeps it.
                return;
            } else return error.UnhandledGenericMappingTypeSubstitution;
        }
    }

    switch (t_info) {
        .pointer => if (t_info.pointer.size == .one) {
            try mapGenerics(alloc, map, node.*);
        } else if (t_info.pointer.size == .slice) {
            for (node.*) |*item| try mapGenerics(alloc, map, item);
        },
        .@"struct" => inline for (t_info.@"struct".fields) |field| {
            if (comptime std.mem.eql(u8, field.name, "pos")) continue;
            try mapGenerics(alloc, map, &@field(node.*, field.name));
        },
        .@"union" => switch (node.*) {
            inline else => |*payload| try mapGenerics(alloc, map, payload),
        },
        .optional => if (node.*) |*payload| try mapGenerics(alloc, map, payload),
        else => {},
    }
}

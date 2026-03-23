const std = @import("std");
const utils = @import("utils");

const ast = @import("ast.zig");

pub const Type = union(enum) {
    const Reference = struct {
        pos: usize,
        inner: *const Type,
        is_mut: bool,
    };

    const Slice = Reference;

    const Array = struct {
        pos: usize,
        inner: *const Type,
        /// if size is `_`, type is an array of inferred size.
        /// if size is a valid expression, type is an array of specified size.
        size: *const ast.Expression,
    };

    const ErrorUnion = struct {
        pos: usize,
        success: *const Type,
        failure: *const Type,
    };

    const Function = struct {
        pos: usize,
        parameters: []const Type,
        generic_parameters: []const Type,
        return_type: *const Type,
    };

    const Generic = struct {
        pos: usize,
        lhs: *const Type,
        arguments: ast.ArgumentList,
    };

    const Member = struct {
        pos: usize,
        parent: *const Type,
        member_name: []const u8,
    };

    inferred: struct { pos: usize },
    symbol: struct { pos: usize, inner: []const u8 },
    optional: struct { pos: usize, inner: *const Type },
    slice: Slice,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,
    generic: Generic,
    variadic: struct { pos: usize },
    member: Member,

    pub inline fn pos(self: Type) usize {
        return switch (self) {
            inline else => |some| some.pos,
        };
    }

    pub fn clonePtr(self: Type, alloc: std.mem.Allocator) !*Type {
        const ret = try alloc.create(Type);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Type, alloc: std.mem.Allocator) std.mem.Allocator.Error!Type {
        return switch (self) {
            .inferred => |n| .{ .inferred = .{ .pos = n.pos } },
            .symbol => |n| .{
                .symbol = .{
                    .pos = n.pos,
                    .inner = try alloc.dupe(u8, n.inner),
                },
            },
            .optional => |n| .{
                .optional = .{
                    .pos = n.pos,
                    .inner = try n.inner.clonePtr(alloc),
                },
            },
            inline .reference, .slice => |n, t| @unionInit(Type, @tagName(t), .{
                .pos = n.pos,
                .inner = try n.inner.clonePtr(alloc),
                .is_mut = n.is_mut,
            }),
            .array => |n| .{
                .array = .{
                    .pos = n.pos,
                    .inner = try n.inner.clonePtr(alloc),
                    .size = try n.size.clonePtr(alloc),
                },
            },
            .error_union => |n| .{
                .error_union = .{
                    .pos = n.pos,
                    .failure = try n.failure.clonePtr(alloc),
                    .success = try n.success.clonePtr(alloc),
                },
            },
            .function => |n| .{
                .function = .{
                    .pos = n.pos,
                    .parameters = try utils.cloneSlice(Type, n.parameters, alloc),
                    .generic_parameters = try utils.cloneSlice(Type, n.generic_parameters, alloc),
                    .return_type = try n.return_type.clonePtr(alloc),
                },
            },
            .generic => |n| .{
                .generic = .{
                    .pos = n.pos,
                    .lhs = try n.lhs.clonePtr(alloc),
                    .arguments = try utils.cloneSlice(ast.Expression, n.arguments, alloc),
                },
            },
            .variadic => |n| .{ .variadic = .{ .pos = n.pos } },
            .member => |n| .{
                .member = .{
                    .pos = n.pos,
                    .parent = try n.parent.clonePtr(alloc),
                    .member_name = try alloc.dupe(u8, n.member_name),
                },
            },
        };
    }

    fn deinitPtr(self: *const Type, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Type, alloc: std.mem.Allocator) void {
        switch (self) {
            else => {},
            .symbol => |s| alloc.free(s.inner),
            .optional => |s| s.inner.deinitPtr(alloc),
            .slice, .reference => |s| s.inner.deinitPtr(alloc),
            .array => |s| {
                s.inner.deinitPtr(alloc);
                s.size.deinitPtr(alloc);
            },
            .error_union => |s| {
                s.failure.deinitPtr(alloc);
                s.success.deinitPtr(alloc);
            },
            .function => |s| {
                utils.deinitSlice(Type, s.parameters, alloc);
                utils.deinitSlice(Type, s.generic_parameters, alloc);
                s.return_type.deinitPtr(alloc);
            },
            .generic => |s| {
                s.lhs.deinitPtr(alloc);
                utils.deinitSlice(ast.Expression, s.arguments, alloc);
            },
            .member => |s| {
                s.parent.deinitPtr(alloc);
                alloc.free(s.member_name);
            },
        }
    }
};

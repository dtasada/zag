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
            .array => |n| {
                const inner = try n.inner.clonePtr(alloc);
                errdefer inner.deinitPtr(alloc);

                const size = try n.size.clonePtr(alloc);
                errdefer size.deinitPtr(alloc);

                return .{
                    .array = .{
                        .pos = n.pos,
                        .inner = inner,
                        .size = size,
                    },
                };
            },
            .error_union => |n| {
                const failure = try n.failure.clonePtr(alloc);
                errdefer failure.deinitPtr(alloc);

                const success = try n.success.clonePtr(alloc);
                errdefer success.deinitPtr(alloc);

                return .{
                    .error_union = .{
                        .pos = n.pos,
                        .failure = failure,
                        .success = success,
                    },
                };
            },
            .function => |n| {
                const parameters = try utils.cloneSlice(Type, n.parameters, alloc);
                errdefer utils.deinitSlice(Type, parameters, alloc);

                const generic_parameters = try utils.cloneSlice(Type, n.generic_parameters, alloc);
                errdefer utils.deinitSlice(Type, generic_parameters, alloc);

                const return_type = try n.return_type.clonePtr(alloc);
                errdefer return_type.deinitPtr(alloc);

                return .{
                    .function = .{
                        .pos = n.pos,
                        .parameters = parameters,
                        .generic_parameters = generic_parameters,
                        .return_type = return_type,
                    },
                };
            },
            .generic => |n| {
                const lhs = try n.lhs.clonePtr(alloc);
                errdefer lhs.deinit(alloc);

                const arguments = try utils.cloneSlice(ast.Expression, n.arguments, alloc);
                errdefer utils.deinitSlice(ast.Expression, arguments, alloc);

                return .{
                    .generic = .{
                        .pos = n.pos,
                        .lhs = lhs,
                        .arguments = arguments,
                    },
                };
            },
            .variadic => |n| .{ .variadic = .{ .pos = n.pos } },
            .member => |n| {
                const parent = try n.parent.clonePtr(alloc);
                errdefer parent.deinitPtr(alloc);

                const member_name = try alloc.dupe(u8, n.member_name);
                errdefer alloc.free(member_name);

                return .{
                    .member = .{
                        .pos = n.pos,
                        .parent = parent,
                        .member_name = member_name,
                    },
                };
            },
        };
    }

    pub fn deinitPtr(self: *const Type, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Type, alloc: std.mem.Allocator) void {
        switch (self) {
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
            else => {},
        }
    }
};

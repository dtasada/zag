const std = @import("std");
const utils = @import("utils");

const ast = @import("ast.zig");

pub const Type = union(enum) {
    const Reference = struct {
        pos: utils.Position,
        inner: *const Type,
        is_mut: bool,
    };

    const Slice = Reference;

    const Array = struct {
        pos: utils.Position,
        inner: *const Type,
        /// if size is `_`, type is an array of inferred size.
        /// if size is a valid expression, type is an array of specified size.
        size: *const ast.Expression,
    };

    const ErrorUnion = struct {
        pos: utils.Position,
        success: *const Type,
        failure: ?*const Type = null,
    };

    const Function = struct {
        pos: utils.Position,
        name: []const u8,
        parameters: ast.ParameterList,
        generic_parameters: ast.ParameterList,
        return_type: *const Type,
    };

    const Generic = struct {
        pos: utils.Position,
        lhs: *const Type,
        arguments: ast.ArgumentList,
    };

    const Member = struct {
        pos: utils.Position,
        parent: *const Type,
        member_name: []const u8,
    };

    inferred: struct { pos: utils.Position },
    symbol: struct { pos: utils.Position, inner: []const u8 },
    optional: struct { pos: utils.Position, inner: *const Type },
    slice: Slice,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,
    generic: Generic,
    variadic: struct { pos: utils.Position },
    member: Member,

    pub inline fn getPosition(self: Type) utils.Position {
        return switch (self) {
            inline else => |some| some.pos,
        };
    }

    fn clonePtr(self: Type, alloc: std.mem.Allocator) !*Type {
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
                    .success = try n.success.clonePtr(alloc),
                    .failure = if (n.failure) |f| try f.clonePtr(alloc) else null,
                },
            },
            .function => |n| .{
                .function = .{
                    .pos = n.pos,
                    .name = try alloc.dupe(u8, n.name),
                    .parameters = try utils.cloneSlice(ast.VariableSignature, n.parameters, alloc),
                    .generic_parameters = try utils.cloneSlice(ast.VariableSignature, n.generic_parameters, alloc),
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
                s.success.deinitPtr(alloc);
                if (s.failure) |f| f.deinitPtr(alloc);
            },
            .function => |s| {
                alloc.free(s.name);
                utils.deinitSlice(ast.VariableSignature, s.parameters, alloc);
                utils.deinitSlice(ast.VariableSignature, s.generic_parameters, alloc);
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

    pub fn createFunctionType(alloc: std.mem.Allocator, pos: utils.Position, name: []const u8, parameters: ast.ParameterList, generic_parameters: ast.ParameterList, return_type: *const Type) !Type {
        return .{
            .function = .{
                .pos = pos,
                .name = try alloc.dupe(u8, name),
                .parameters = parameters,
                .generic_parameters = generic_parameters,
                .return_type = return_type,
            },
        };
    }
};

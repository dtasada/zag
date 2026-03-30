const std = @import("std");
const ast = @import("ast");

const compiler = @import("compiler.zig");

const Compiler = compiler.Compiler;
const Type = compiler.Type;

pub const Value = union(enum) {
    uint: usize,
    int: isize,
    float: f64,
    bool: bool,

    type: Type,
    nil,
    undefined,

    pub fn eval(expr: *const ast.Expression, c: *const Compiler) !Value {
        _ = c;
        return switch (expr.*) {
            .int => |int| .{ .uint = int.payload },
            else => @panic("unimplemented"),
        };
    }

    pub fn getType(self: Value) Type {
        return switch (self) {
            .uint => .usize,
            .int => .isize,
            .float => .f64,
            .bool => .bool,
            .type => .type,
            .nil => .@"typeof(nil)",
            .undefined => .@"typeof(undefined)",
        };
    }

    pub fn eql(lhs: Value, rhs: Value) bool {
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

        return switch (lhs) {
            inline .uint, .int, .float, .bool => |a, tag| a == @field(rhs, @tagName(tag)),
            .type => lhs.type.eql(rhs.type),
            .nil => true,
            .undefined => false,
        };
    }

    pub fn deinit(self: Value, alloc: std.mem.Allocator) void {
        switch (self) {
            .type => |t| t.deinit(alloc),
            else => {},
        }
    }

    pub fn clone(self: Value, alloc: std.mem.Allocator) !Value {
        return switch (self) {
            .type => |t| .{ .type = try t.clone(alloc) },
            else => self,
        };
    }
};

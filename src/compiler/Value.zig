const std = @import("std");
const ast = @import("ast");

const compiler = @import("compiler.zig");
const errors = @import("errors.zig");

const Compiler = compiler.Compiler;
const Type = compiler.Type;
const Error = errors.Error;

pub const Value = union(enum) {
    uint: usize,
    int: isize,
    float: f64,
    bool: bool,

    type: Type,
    nil,
    undefined,

    pub fn eval(
        alloc: std.mem.Allocator,
        io: std.Io,
        expr: *const ast.Expression,
        c: *Compiler,
        m: *const compiler.Module,
    ) Error!Value {
        return switch (expr.*) {
            .int => |int| .{ .uint = int.payload },
            .ident => |ident| {
                const symbol = c.module.getSymbol(ident.payload) orelse
                    return errors.unknownSymbol(io, ident.payload, m.source_map[ident.pos]);
                if (symbol.type == .type)
                    return .{ .type = try symbol.value.?.type.clone(alloc) };
                if (symbol.value) |v| return try v.clone(alloc);
                return error.UnknownSymbol; // Should probably have a better error for this
            },
            .type => |t| .{ .type = try Type.fromAst(alloc, io, &t.payload, c, m) },
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

    pub fn clone(self: Value, alloc: std.mem.Allocator) Error!Value {
        return switch (self) {
            .type => |t| .{ .type = try t.clone(alloc) },
            else => self,
        };
    }

    pub fn format(self: Value, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            inline .uint, .int, .float, .bool => |v| try writer.print("{}", .{v}),
            inline .nil, .undefined => |_, t| _ = try writer.write(@tagName(t)),
            .type => |t| try writer.print("{f}", .{t}),
        }
    }
};

const std = @import("std");

const ast = @import("Parser").ast;

const Type = @import("Type.zig").Type;

pub const Value = union(enum) {
    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,

    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,

    f32: f32,
    f64: f64,

    bool: bool,

    type: Type,

    void,

    @"struct": CompoundType(.@"struct"),
    @"enum": CompoundType(.@"enum"),
    @"union": CompoundType(.@"union"),
    optional: struct {
        type: Type,
        value: *const Value,
    },
    reference: struct { value: *const Value, type: Type.Reference },
    array: struct { value: *const Value, type: Type.Array },
    error_union: struct { value: *const Value, type: Type.ErrorUnion },
    function: struct { value: *const Value, type: Type.Function },

    fn CompoundType(compound_type: enum { @"struct", @"union", @"enum" }) type {
        return struct {
            type: switch (compound_type) {
                .@"struct" => Type.Struct,
                .@"union" => Type.Union,
                .@"enum" => Type.Enum,
            },
            members: std.StringHashMap(*const Value),
            methods: std.StringHashMap(*const Value),
        };
    }

    pub fn getType(self: *const Value) Type {
        return switch (self.*) {
            .i8 => .i8,
            .i16 => .i16,
            .i32 => .i32,
            .i64 => .i64,
            .u8 => .u8,
            .u16 => .u16,
            .u32 => .u32,
            .u64 => .u64,
            .f32 => .f32,
            .f64 => .f64,
            .bool => .bool,
            .type => |*t| .{ .type = t },
            .void => .void,
            .@"struct" => |s| .{ .@"struct" = s.type },
            .@"union" => |u| .{ .@"union" = u.type },
            .@"enum" => |e| .{ .@"enum" = e.type },
            .optional => |opt| opt.type,
            .reference => |ref| .{ .reference = ref.type },
            .array => |arr| .{ .array = arr.type },
            .error_union => |err| .{ .error_union = err.type },
            .function => |func| .{ .function = func.type },
        };
    }

    pub fn binaryOperation(lhs: Value, op: ast.BinaryOperator, rhs: Value) !Value {
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs))
            @panic("invalid binary operation: the two values are not of the same type\n");

        const switch_fn = (struct {
            fn switchFn(comptime tag: []const u8, inner_lhs: anytype, inner_op: ast.BinaryOperator, inner_rhs: anytype) !Value {
                return @unionInit(Value, tag, switch (@typeInfo(@TypeOf(inner_lhs))) {
                    .int => switch (inner_op) {
                        .@"+" => inner_lhs + inner_rhs,
                        .@"-" => inner_lhs - inner_rhs,
                        .@"*" => inner_lhs * inner_rhs,
                        .@"/" => @divTrunc(inner_lhs, inner_rhs),
                        .@"%" => @mod(inner_lhs, inner_rhs),

                        .@"==" => return .{ .bool = inner_lhs == inner_rhs },
                        .@">" => return .{ .bool = inner_lhs > inner_rhs },
                        .@"<" => return .{ .bool = inner_lhs < inner_rhs },
                        .@">=" => return .{ .bool = inner_lhs >= inner_rhs },
                        .@"<=" => return .{ .bool = inner_lhs <= inner_rhs },
                        .@"!=" => return .{ .bool = inner_lhs != inner_rhs },

                        .@"&" => inner_lhs & inner_rhs,
                        .@"|" => inner_lhs | inner_rhs,
                        .@"^" => inner_lhs ^ inner_rhs,
                        .@">>" => inner_lhs >> @as(u6, @intCast(inner_rhs)),
                        .@"<<" => inner_lhs << @as(u6, @intCast(inner_rhs)),
                        else => unreachable,
                    },
                    .float => switch (inner_op) {
                        .@"+" => inner_lhs + inner_rhs,
                        .@"-" => inner_lhs - inner_rhs,
                        .@"*" => inner_lhs * inner_rhs,
                        .@"/" => @divTrunc(inner_lhs, inner_rhs),
                        .@"%" => @mod(inner_lhs, inner_rhs),

                        .@"==" => return .{ .bool = inner_lhs == inner_rhs },
                        .@">" => return .{ .bool = inner_lhs > inner_rhs },
                        .@"<" => return .{ .bool = inner_lhs < inner_rhs },
                        .@">=" => return .{ .bool = inner_lhs >= inner_rhs },
                        .@"<=" => return .{ .bool = inner_lhs <= inner_rhs },
                        .@"!=" => return .{ .bool = inner_lhs != inner_rhs },
                        else => unreachable,
                    },
                    .bool => switch (inner_op) {
                        .@"==" => inner_lhs == inner_rhs,
                        .@"!=" => inner_lhs != inner_rhs,

                        .@"&" => inner_lhs & inner_rhs,
                        .@"|" => inner_lhs | inner_rhs,
                        .@"^" => inner_lhs ^ inner_rhs,
                        .@"and" => inner_lhs and inner_rhs,
                        .@"or" => inner_lhs or inner_rhs,
                        else => unreachable,
                    },
                    else => unreachable,
                });
            }
        }).switchFn;

        return switch (lhs) {
            .i64, .i32, .i16, .i8 => |lhs_int| switch (rhs) {
                .i64, .i32, .i16, .i8 => |rhs_int| try switch_fn("i64", lhs_int, op, rhs_int),
                else => @panic("invalid binary operation: the two values are not of numeric boolean type\n"),
            },
            .u64, .u32, .u16, .u8 => |lhs_uint| switch (rhs) {
                .u64, .u32, .u16, .u8 => |rhs_uint| try switch_fn("u64", lhs_uint, op, rhs_uint),
                else => @panic("invalid binary operation: the two values are not of numeric boolean type\n"),
            },
            .f64, .f32 => |lhs_float| switch (rhs) {
                .f64, .f32 => |rhs_float| try switch_fn("f64", lhs_float, op, rhs_float),
                else => @panic("invalid binary operation: the two values are not of numeric boolean type\n"),
            },
            .bool => |lhs_bool| switch (rhs) {
                .bool => |rhs_bool| try switch_fn("bool", lhs_bool, op, rhs_bool),
                else => @panic("invalid binary operation: the two values are not of numeric or boolean type\n"),
            },
            else => @panic("invalid binary operation: the two values are not of numeric or boolean type\n"),
        };
    }

    pub fn hash(self: Value) u64 {
        var h = std.hash.Wyhash.init(0);
        const tag = std.meta.activeTag(self);
        h.update(std.mem.asBytes(&tag));
        switch (self) {
            inline .i8,
            .u8,
            .bool,
            .i16,
            .u16,
            .i32,
            .u32,
            .f32,
            .i64,
            .u64,
            .f64,
            => |v| h.update(std.mem.asBytes(&v)),
            .type => |t| h.update(std.mem.asBytes(&t.hash())),
            .void => {},
            else => @panic("TODO: hash for complex values"),
        }
        return h.final();
    }
};

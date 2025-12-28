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

    pub fn binaryOperation(lhs: Value, op: ast.BinaryOperator, rhs: Value) !Value {
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs))
            @panic("invalid binary operation: the two values are not of the same type\n");

        const switch_fn = (struct {
            fn switchFn(inner_lhs: anytype, inner_op: ast.BinaryOperator, inner_rhs: anytype) !Value {
                return switch (inner_op) {
                    .plus => inner_lhs + inner_rhs,
                    .dash => inner_lhs - inner_rhs,
                    .asterisk => inner_lhs * inner_rhs,
                    .slash => @divTrunc(inner_lhs, inner_rhs),
                    .percent => @mod(inner_lhs, inner_rhs),

                    .equals_equals => inner_lhs == inner_rhs,
                    .greater => inner_lhs > inner_rhs,
                    .less => inner_lhs < inner_rhs,
                    .greater_equals => inner_lhs >= inner_rhs,
                    .less_equals => inner_lhs <= inner_rhs,
                    .bang_equals => inner_lhs != inner_rhs,

                    .ampersand => inner_lhs & inner_rhs,
                    .pipe => inner_lhs | inner_rhs,
                    .caret => inner_lhs ^ inner_rhs,
                    .logical_and => inner_lhs and inner_rhs,
                    .logical_or => inner_lhs or inner_rhs,
                    .shift_right => inner_lhs >> inner_rhs,
                    .shift_left => inner_lhs << inner_rhs,
                };
            }
        }).switchFn;

        const result = switch (lhs) {
            .i64, .i32, .i16, .i8 => |lhs_int| switch (rhs) {
                .i64, .i32, .i16, .i8 => |rhs_int| try switch_fn(lhs_int, op, rhs_int),
                else => @panic("invalid binary operation: the two values are not of numeric boolean type\n"),
            },
            .u64, .u32, .u16, .u8 => |lhs_uint| switch (rhs) {
                .u64, .u32, .u16, .u8 => |rhs_uint| try switch_fn(lhs_uint, op, rhs_uint),
                else => @panic("invalid binary operation: the two values are not of numeric boolean type\n"),
            },
            .f64, .f32 => |lhs_float| switch (rhs) {
                .f64, .f32 => |rhs_float| try switch_fn(lhs_float, op, rhs_float),
                else => @panic("invalid binary operation: the two values are not of numeric boolean type\n"),
            },
            .bool => |lhs_bool| switch (rhs) {
                .bool => |rhs_bool| try switch_fn(lhs_bool, op, rhs_bool),
                else => @panic("invalid binary operation: the two values are not of numeric or boolean type\n"),
            },
            else => @panic("invalid binary operation: the two values are not of numeric or boolean type\n"),
        };

        return switch (lhs) {
            .i64 => .{ .i64 = result },
            .u64 => .{ .u64 = result },
            .f64 => .{ .f64 = result },
        };
    }
};

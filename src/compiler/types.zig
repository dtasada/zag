const std = @import("std");

pub const primitives: std.static_string_map.StaticStringMap([]const u8) = .initComptime(.{
    .{ "i8", "int8_t" },
    .{ "i16", "int16_t" },
    .{ "i32", "int32_t" },
    .{ "i64", "int64_t" },

    .{ "u8", "uint8_t" },
    .{ "u16", "uint16_t" },
    .{ "u32", "uint32_t" },
    .{ "u64", "uint64_t" },

    .{ "void", "void" },
    // .{ "string", "char*" },
});

pub fn get(t: []const u8) ![]const u8 {
    return primitives.get(t) orelse error.UndeclaredType;
}

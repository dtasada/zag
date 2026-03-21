const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");
const Module = @import("Module.zig");

pub const Type = union(enum) {
    void,
    bool,
    type,

    i8,
    i16,
    i32,
    i64,
    isize,

    u8,
    u16,
    u32,
    u64,
    usize,

    c_char,
    c_short,
    c_int,
    c_long,

    c_uchar,
    c_ushort,
    c_uint,
    c_ulong,

    optional: *const Type,
    reference: Reference,
    slice: Slice,
    array: Array,
    error_union: ErrorUnion,

    function: Function,

    @"struct": Struct,
    @"enum": Enum,
    @"union": Union,

    module: Module,

    const Reference = struct { inner: *const Type, is_mut: bool };
    const Slice = struct { inner: *const Type, is_mut: bool };
    const Array = struct { inner: *const Type, len: usize };
    const ErrorUnion = struct { failure: *const Type, success: *const Type };

    const Struct = CompoundType(.@"struct");
    const Enum = CompoundType(.@"enum");
    const Union = CompoundType(.@"union");

    fn CompoundType(tag: utils.CompoundTypeTag) type {
        return struct {
            const Member = switch (tag) {
                .@"enum" => usize,
                else => Type,
            };

            members: std.StringHashMap(Member),
            methods: std.StringHashMap(Function),
            variables: std.StringHashMap(Function),
        };
    }

    const Function = struct {
        const Parameter = struct {
            name: []const u8,
            type: Type,
        };

        parameters: std.ArrayList(Parameter),
        return_type: *const Type,
    };

    pub const GenericTemplate = struct {
        params: []const Param,
        definition: *const ast.Statement,
        module: Module,

        pub const Param = struct {
            name: []const u8,
            constraint: ?Type = null,
        };
    };
};

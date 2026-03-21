const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const errors = @import("errors.zig");
const compiler = @import("compiler.zig");

const Compiler = compiler.Compiler;
const Module = compiler.Module;
const Value = compiler.Value;

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

    fn fromAstPtr(alloc: std.mem.Allocator, t: ast.Type, c: Compiler) !*Type {
        const ret = try alloc.create(Type);
        ret.* = fromAst(alloc, t, c);
        return ret;
    }

    pub fn fromAst(alloc: std.mem.Allocator, t: ast.Type, c: Compiler) !Type {
        switch (t) {
            .inferred => unreachable,
            .symbol => |symbol| c.module.getSymbol(symbol.inner) orelse
                errors.unknownSymbol(symbol.inner, c.source_map[symbol.pos]),
            .optional => |opt| .{ .optional = try fromAstPtr(alloc, opt.inner, c) },
            inline .slice, .reference => |ref, tag| @unionInit(Type, @tagName(tag), .{
                .inner = try fromAstPtr(alloc, ref.inner, c),
                .is_mut = ref.is_mut,
            }),
            .array => |array| .{
                .array = .{
                    .inner = try fromAstPtr(alloc, array.inner, c),
                    .len = switch (try Value.eval(array.size)) {
                        .uint => |uint| uint,
                        else => |val| return errors.arrayLengthMustBeInteger(val.getType(), array.size.getPosition()),
                    },
                },
            },
            .error_union => |eu| .{
                .error_union = .{
                    .failure = try fromAstPtr(alloc, eu.failure, c),
                    .success = try fromAstPtr(alloc, eu.failure, c),
                },
            },
            // .function => |f| .{
            //     .function = .{},
            // },
            else => unreachable,
        }
    }
};

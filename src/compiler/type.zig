const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const errors = @import("errors.zig");
const compiler = @import("compiler.zig");

const Compiler = compiler.Compiler;
const Symbol = compiler.Symbol;
const Module = compiler.Module;
const Value = compiler.Value;
const Error = errors.Error;

pub const Type = union(enum) {
    void,
    bool,
    type,
    variadic,

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

    f32,
    f64,

    c_char,
    c_short,
    c_int,
    c_long,

    c_uchar,
    c_ushort,
    c_uint,
    c_ulong,

    c_float,
    c_double,

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
            const Member = if (tag == .@"enum") struct {
                name: []const u8,
                value: usize,
                pub fn deinit(_: Member, _: std.mem.Allocator) void {}
            } else struct {
                name: []const u8,
                type: Type,
                pub fn deinit(self: Member, alloc: std.mem.Allocator) void {
                    self.type.deinit(alloc);
                }
            };

            name: []const u8,
            members: []const Member,
            symbols: []const Symbol,
        };
    }

    const Function = struct {
        parameters: []const Type,
        return_type: *const Type,

        pub fn deinit(self: Function, alloc: std.mem.Allocator) void {
            utils.deinitSlice(Type, self.parameters, alloc);
            self.return_type.deinitPtr(alloc);
        }
    };

    pub fn fromAstPtr(alloc: std.mem.Allocator, t: ast.Type, c: *const Compiler) !*Type {
        const ret = try alloc.create(Type);
        ret.* = try fromAst(alloc, t, c);
        return ret;
    }

    pub fn fromAst(alloc: std.mem.Allocator, t: ast.Type, c: *const Compiler) Error!Type {
        return switch (t) {
            .symbol => |s| {
                const symbol = c.module.getSymbol(s.inner) orelse
                    return errors.unknownSymbol(s.inner, c.source_map[s.pos]);

                return switch (symbol.type) {
                    .type => symbol.value.?.type,
                    else => |received| errors.typeMismatch(.type, received, c.source_map[s.pos]),
                };
            },
            .optional => |opt| .{ .optional = try fromAstPtr(alloc, opt.inner.*, c) },
            inline .slice, .reference => |ref, tag| @unionInit(Type, @tagName(tag), .{
                .inner = try fromAstPtr(alloc, ref.inner.*, c),
                .is_mut = ref.is_mut,
            }),
            .array => |array| .{
                .array = .{
                    .inner = try fromAstPtr(alloc, array.inner.*, c),
                    .len = switch (try Value.eval(array.size, c)) {
                        .uint => |uint| uint,
                        else => |val| return errors.arrayLengthMustBeInteger(
                            val.getType(),
                            c.source_map[array.size.pos()],
                        ),
                    },
                },
            },
            .error_union => |eu| .{
                .error_union = .{
                    .failure = try fromAstPtr(alloc, eu.failure.*, c),
                    .success = try fromAstPtr(alloc, eu.success.*, c),
                },
            },
            .function => |f| {
                var params: std.ArrayList(Type) = try .initCapacity(alloc, f.parameters.len);
                errdefer utils.deinitArrayList(Type, &params, alloc);
                for (f.parameters) |param| params.appendAssumeCapacity(try fromAst(alloc, param, c));

                const return_type = try fromAstPtr(alloc, f.return_type.*, c);
                errdefer return_type.deinitPtr(alloc);

                return .{
                    .function = .{
                        .parameters = try params.toOwnedSlice(alloc),
                        .return_type = return_type,
                    },
                };
            },
            .variadic => .variadic,
            inline else => |_, tag| {
                std.debug.print("failing with {s}\n", .{@tagName(tag)});
                unreachable;
            },
        };
    }

    pub fn deinitPtr(self: *const Type, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Type, alloc: std.mem.Allocator) void {
        switch (self) {
            .optional => |opt| opt.deinitPtr(alloc),
            .error_union => |eu| {
                eu.failure.deinitPtr(alloc);
                eu.success.deinitPtr(alloc);
            },
            .function => |f| f.deinit(alloc),
            inline .reference, .slice, .array => |t| t.inner.deinitPtr(alloc),
            inline .@"struct", .@"enum", .@"union" => |ct| {
                utils.deinitSlice(@TypeOf(ct).Member, ct.members, alloc);
                utils.deinitSlice(Symbol, ct.symbols, alloc);
            },
            else => {},
        }
    }

    pub fn format(self: Type, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .optional => |inner| try writer.print("?{f}", .{inner.*}),
            .reference => |ref| try writer.print("&{s}{f}", .{ if (ref.is_mut) "mut " else "", ref.inner.* }),
            .slice => |slice| try writer.print("[]{s}{f}", .{ if (slice.is_mut) "mut " else "", slice.inner.* }),
            .array => |array| try writer.print("[{}]{f}", .{ array.len, array.inner.* }),
            .error_union => |eu| try writer.print("{f}!{f}", .{ eu.failure.*, eu.success.* }),
            .function => |f| {
                try writer.writeAll("fn (");
                for (f.parameters, 0..) |param, i| {
                    try writer.print("{f}", .{param});
                    try writer.writeAll(if (i == f.parameters.len - 1) ")" else ", ");
                }
            },
            inline .@"struct", .@"enum", .@"union" => |ct| try writer.writeAll(ct.name),
            .module => |m| try writer.writeAll(m.name),
            inline else => |_, t| try writer.writeAll(@tagName(t)),
        }
    }

    pub fn hash(self: Type) u64 {
        var h = std.hash.Wyhash.init(0);

        h.update(std.mem.asBytes(&std.meta.activeTag(self)));

        switch (self) {
            .optional => |inner| h.update(std.mem.asBytes(&inner.hash())),
            inline .reference, .slice => |ref| {
                h.update(std.mem.asBytes(&ref.inner.hash()));
                h.update(std.mem.asBytes(&ref.is_mut));
            },
            .array => |array| {
                h.update(std.mem.asBytes(&array.inner.hash()));
                h.update(std.mem.asBytes(&array.len));
            },
            .error_union => |eu| {
                h.update(std.mem.asBytes(&eu.failure.hash()));
                h.update(std.mem.asBytes(&eu.success.hash()));
            },
            .function => |f| {
                h.update(std.mem.asBytes(&f.return_type.hash()));
                for (f.parameters) |param_t| h.update(std.mem.asBytes(&param_t.hash()));
            },
            inline .@"struct", .@"enum", .@"union" => |ct, tag| {
                h.update(std.mem.asBytes(&tag));
                for (ct.members) |member| {
                    h.update(member.name);
                    h.update(std.mem.asBytes(&if (tag == .@"enum") member else member.type.hash()));
                }
            },
            else => {},
        }

        return h.final();
    }
};

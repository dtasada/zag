const std = @import("std");

const ast = @import("Parser").ast;

const expressions = @import("expressions.zig");

const Compiler = @import("Compiler.zig");
const CompilerError = Compiler.CompilerError;

pub const Type = union(enum) {
    const Self = @This();

    pub const Function = struct {
        params: std.ArrayList(*const Self),
        return_type: *const Self,
    };

    fn CompoundType(T: enum { @"struct", @"enum", @"union" }) type {
        return struct {
            pub const MemberType = switch (T) {
                .@"struct", .@"union" => *const Self,
                .@"enum" => ?usize,
            };

            const Method = struct {
                inner_name: []const u8,
                params: std.ArrayList(*const Self),
                return_type: *const Self,
            };

            name: []const u8,
            members: *std.StringHashMap(MemberType),
            methods: *std.StringHashMap(Method),

            /// get member or method. returns `null` if no member or method is found with `name`.
            pub fn getProperty(self: *const CompoundType(T), name: []const u8) ?union(enum) {
                member: MemberType,
                method: Method,
            } {
                const member = self.members.get(name);
                const method = self.methods.get(name);

                return if (member) |m|
                    .{ .member = m }
                else if (method) |m|
                    .{ .method = m }
                else
                    null;
            }

            pub fn init(alloc: std.mem.Allocator, name: []const u8) !CompoundType(T) {
                const members = try alloc.create(std.StringHashMap(MemberType));
                members.* = .init(alloc);
                const methods = try alloc.create(std.StringHashMap(Method));
                methods.* = .init(alloc);
                return .{
                    .name = name,
                    .members = members,
                    .methods = methods,
                };
            }

            pub fn eq(lhs: *const Struct, rhs: *const Struct) bool {
                inline for (@typeInfo(Struct).@"struct".fields) |field| {
                    const pa: *const field.type = @ptrCast(&@field(lhs.*, field.name));
                    const pb: *const field.type = @ptrCast(&@field(rhs.*, field.name));
                    if (!deepEqInternal(field.type, pa, pb)) return false;
                }
                return true;

                // return Type.eq(
                //     &.{
                //         .@"struct" = .{
                //             .name = rhs.name,
                //             .members = rhs.members,
                //             .methods = rhs.methods,
                //         },
                //     },
                //     &.{
                //         .@"struct" = .{
                //             .name = lhs.name,
                //             .members = lhs.members,
                //             .methods = lhs.methods,
                //         },
                //     },
                // );
            }
        };
    }

    pub const Struct = CompoundType(.@"struct");
    pub const Union = CompoundType(.@"union");
    pub const Enum = CompoundType(.@"enum");

    pub const Reference = struct {
        inner: *const Self,
        is_mut: bool,
    };

    pub const Array = struct {
        inner: *const Self,
        /// if size is `null` type is an arraylist, else it's an array.
        /// if size is `_`, type is an array of inferred size.
        /// if size is a valid expression, type is an array of specified size.
        size: ?usize = null,
    };

    pub const ErrorUnion = struct {
        success: *const Self,
        @"error": ?*const Self = null,
    };

    i8,
    i16,
    i32,
    i64,

    u8,
    u16,
    u32,
    u64,

    f32,
    f64,

    bool,

    void,

    @"struct": Struct,
    @"enum": Enum,
    @"union": Union,
    optional: *const Self,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,

    pub fn fromSymbol(symbol: []const u8) !Self {
        return if (std.mem.eql(u8, symbol, "i8"))
            .i8
        else if (std.mem.eql(u8, symbol, "i16"))
            .i16
        else if (std.mem.eql(u8, symbol, "i32"))
            .i32
        else if (std.mem.eql(u8, symbol, "i64"))
            .i64
        else if (std.mem.eql(u8, symbol, "u8"))
            .u8
        else if (std.mem.eql(u8, symbol, "u16"))
            .u16
        else if (std.mem.eql(u8, symbol, "u32"))
            .u32
        else if (std.mem.eql(u8, symbol, "u64"))
            .u64
        else if (std.mem.eql(u8, symbol, "f32"))
            .f32
        else if (std.mem.eql(u8, symbol, "f64"))
            .f64
        else if (std.mem.eql(u8, symbol, "void"))
            .void
        else if (std.mem.eql(u8, symbol, "bool"))
            .bool
        else
            error.TypeNotPrimitive;
    }

    /// Converts an AST type to a Compiler type.
    /// `infer` is the expression with which the type is inferred.
    pub fn fromAst(compiler: *Compiler, t: ast.Type) Compiler.CompilerError!Self {
        return switch (t) {
            .symbol => |symbol| fromSymbol(symbol) catch try compiler.getSymbolType(symbol),
            .reference => |reference| .{
                .reference = .{
                    .inner = b: {
                        const ref_type = try compiler.alloc.create(Self);
                        ref_type.* = try fromAst(compiler, reference.inner.*);
                        break :b ref_type;
                    },
                    .is_mut = reference.is_mut,
                },
            },
            .function => |function| .{
                .function = .{
                    .params = b: {
                        var params: std.ArrayList(*const Self) = try .initCapacity(
                            compiler.alloc,
                            function.parameters.items.len,
                        );
                        for (function.parameters.items) |p| {
                            const param = try compiler.alloc.create(Self);
                            param.* = try fromAst(compiler, p.type);
                            params.appendAssumeCapacity(param);
                        }
                        break :b params;
                    },
                    .return_type = b: {
                        const return_type = try compiler.alloc.create(Self);
                        return_type.* = try fromAst(compiler, function.return_type.*);
                        break :b return_type;
                    },
                },
            },
            .array => |array| .{
                .array = .{
                    .inner = b: {
                        const array_type = try compiler.alloc.create(Self);
                        array_type.* = try fromAst(compiler, array.inner.*);
                        break :b array_type;
                    },
                    .size = (try compiler.solveComptimeExpression(if (array.size) |s|
                        s.*
                    else
                        @panic("can't infer array size"))).u64,
                },
            },
            else => |other| std.debug.panic("unimplemented type {s}\n", .{@tagName(other)}),
        };
    }

    pub fn infer(compiler: *Compiler, expr: ast.Expression) CompilerError!Self {
        return switch (expr) {
            .ident => |ident| try compiler.getSymbolType(ident),
            .string => .{ .reference = .{ .inner = &.u8, .is_mut = false } },
            .char => .u8,
            .uint => |uint| if (uint <= std.math.maxInt(i32)) .i32 else .i64,
            .int => |int| if (int <= std.math.maxInt(i32) and int >= std.math.minInt(i32))
                .i32
            else
                .i64,
            .float => |float| if (float == @as(f64, @floatCast(@as(f32, @floatCast(float)))))
                // if the float fits in an f32, then default to f32. if the float is too big,
                // use f64
                .f32
            else
                .f64,

            .call => |call| try inferCallExpression(compiler, call),
            .struct_instantiation => |struct_inst| try compiler.getSymbolType(struct_inst.name),
            .prefix => |prefix| try infer(compiler, prefix.rhs.*),
            .array_instantiation => |array| try inferArrayInstantiationExpression(compiler, array),
            .member => |member| try inferMemberExpression(compiler, member),
            .reference => |reference| .{
                .reference = .{
                    .inner = b: {
                        const inner = try compiler.alloc.create(Type);
                        inner.* = try .infer(compiler, reference.inner.*);
                        break :b inner;
                    },
                    .is_mut = reference.is_mut,
                },
            },
            else => |other| std.debug.panic("unimplemented type: {s}\n", .{@tagName(other)}),
        };
    }

    fn inferCallExpression(compiler: *Compiler, call: ast.Expression.Call) !Self {
        return switch (call.callee.*) {
            .member => |m| {
                var parent_expr_buf: std.ArrayList(u8) = .empty;
                try expressions.compile(compiler, &parent_expr_buf, m.parent, .{}); // TODO fix bug
                const parent_type = try compiler.getSymbolType(parent_expr_buf.items);

                b: switch (parent_type) {
                    .@"struct" => |@"struct"| {
                        if (@"struct".methods.get(m.member_name)) |method| {
                            return method.return_type.*;
                        } else std.debug.panic("comperr: {s}.{s} is not a method\n", .{
                            @"struct".name,
                            m.member_name,
                        });
                    },
                    .reference => |reference| continue :b reference.inner.*,
                    else => |other| std.debug.panic(
                        "comperr: member expression on {s} is illegal\n",
                        .{@tagName(other)},
                    ),
                }
            },
            else => switch (try infer(compiler, call.callee.*)) {
                .function => |function| function.return_type.*,
                else => std.debug.panic("comperr: unimplemented idek\n", .{}),
            },
        };
    }

    fn inferArrayInstantiationExpression(compiler: *Compiler, array: ast.Expression.ArrayInstantiation) !Self {
        const t = try compiler.alloc.create(Type);
        t.* = try .fromAst(compiler, array.type);

        const size = if (array.length.* == .ident and std.mem.eql(u8, array.length.ident, "_"))
            array.contents.items.len
        else b: {
            const length = (try compiler.solveComptimeExpression(array.length.*)).u64;

            if (length != array.contents.items.len)
                std.debug.panic("comperr: array type size does not match initializer list\n", .{});

            break :b length;
        };

        return .{
            .array = .{
                .inner = t,
                .size = size,
            },
        };
    }

    fn inferMemberExpression(compiler: *Compiler, member: ast.Expression.Member) !Self {
        var parent_expr_buf: std.ArrayList(u8) = .empty;
        try expressions.compile(compiler, &parent_expr_buf, member.parent, .{});
        const parent_type = try compiler.getSymbolType(parent_expr_buf.items);

        b: switch (parent_type) {
            .@"struct" => |@"struct"| {
                if (@"struct".getProperty(member.member_name)) |property| switch (property) {
                    .member => |m| return m.*,
                    .method => |method| return .{
                        .function = .{
                            .params = method.params,
                            .return_type = method.return_type,
                        },
                    },
                } else std.debug.panic("comperr: property {s} doesn't exist for type {s}\n", .{
                    member.member_name,
                    @"struct".name,
                });
            },
            .reference => |reference| continue :b reference.inner.*,
            else => |other| std.debug.panic(
                "comperr: member expression on {s} is illegal\n",
                .{@tagName(other)},
            ),
        }
    }

    /// Recursively compares two values of type T for deep equality.
    /// - Handles structs, unions, enums, pointers, slices, arrays, primitives.
    /// - Assumes no cycles in pointers.
    /// - Works for arbitrary nested types.
    pub fn eq(a: *const Self, b: *const Self) bool {
        if (a == b) return true;

        return deepEqInternal(Self, a, b);
    }

    fn deepEqInternal(comptime T: type, a: *const T, b: *const T) bool {
        const info = @typeInfo(T);

        return switch (info) {
            .int, .float, .bool, .comptime_int, .comptime_float => a.* == b.*,
            .pointer => |pointer| {
                if (a == b) return true;
                return deepEqInternal(pointer.child, @ptrCast(a.*), @ptrCast(b.*));
            },
            .array => |array| {
                for (0..array.len) |i| {
                    if (!deepEqInternal(array.child, &a.*[i], &b.*[i])) return false;
                }
                return true;
            },
            .@"struct" => |@"struct"| {
                inline for (@"struct".fields) |field| {
                    const pa: *const field.type = @ptrCast(&@field(a.*, field.name));
                    const pb: *const field.type = @ptrCast(&@field(b.*, field.name));
                    if (!deepEqInternal(field.type, pa, pb)) return false;
                }
                return true;
            },
            .@"union" => |@"union"| {
                if (std.meta.activeTag(a.*) != std.meta.activeTag(b.*)) return false;

                const idx = @intFromEnum(std.meta.activeTag(a.*));

                inline for (@"union".fields, 0..) |field, i| {
                    if (idx == i) {
                        const pa: *const field.type = @ptrCast(&@field(a.*, field.name));
                        const pb: *const field.type = @ptrCast(&@field(b.*, field.name));
                        return deepEqInternal(field.type, pa, pb);
                    }
                }

                unreachable;
            },
            .@"enum" => a.* == b.*,
            .void => true,
            .@"opaque", .@"fn" => {
                std.debug.print("checking type equality for unhandled types! generates ub", .{});
                return false;
            },
            .optional => |opt| {
                const a_val = a.*;
                const b_val = b.*;

                if (a_val == null and b_val == null) return true;
                if (a_val == null or b_val == null) return false;

                return deepEqInternal(opt.child, &a_val.?, &b_val.?);
            },
            else => comptime {
                var buf: [128]u8 = undefined;
                const err = std.fmt.bufPrint(&buf, "Type not supported for deep equality: {s}", .{@typeName(T)}) catch "error creating error message";
                @compileError(err);
            },
        };
    }
};

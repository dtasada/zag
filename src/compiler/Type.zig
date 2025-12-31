const std = @import("std");

const utils = @import("utils");
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

    size,

    f32,
    f64,

    bool,

    void,

    type,

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
        else if (std.mem.eql(u8, symbol, "size"))
            .size
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
            .symbol => |symbol| try compiler.getSymbolType(symbol),
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
            .ident => |ident| compiler.getSymbolType(ident) catch return utils.printErr(
                error.UnknownSymbol,
                "comperr: Unknown symbol '{s}' at {f}\n",
                .{ ident, try compiler.parser.getExprPos(expr) },
                .red,
            ),
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
            .struct_instantiation => |struct_inst| compiler.getSymbolType(struct_inst.name) catch return utils.printErr(
                error.UnknownSymbol,
                "comperr: Unknown symbol '{s}' at {f}\n",
                .{ struct_inst.name, try compiler.parser.getExprPos(expr) },
                .red,
            ),
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
                const parent_type = compiler.getSymbolType(parent_expr_buf.items) catch return utils.printErr(
                    error.UnknownSymbol,
                    "comperr: Unknown symbol '{s}' at {f}\n",
                    .{ parent_expr_buf.items, try compiler.parser.getExprPos(.{ .member = m }) },
                    .red,
                );

                b: switch (parent_type) {
                    .@"struct" => |@"struct"| {
                        if (@"struct".methods.get(m.member_name)) |method| {
                            return method.return_type.*;
                        } else std.debug.panic("comperr: '{s}.{s}' is not a method\n", .{
                            @"struct".name,
                            m.member_name,
                        });
                    },
                    .reference => |reference| continue :b reference.inner.*,
                    else => |other| std.debug.panic(
                        "comperr: Member expression on {f} is illegal\n",
                        .{other},
                    ),
                }
            },
            else => switch (try infer(compiler, call.callee.*)) {
                .function => |function| function.return_type.*,
                else => |t| utils.printErr(
                    error.IllegalExpression,
                    "comperr: Member expression on '{f}' is illegal ({f})\n",
                    .{ t, try compiler.parser.getExprPos(.{ .call = call }) },
                    .red,
                ),
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

            const expected_length = array.contents.items.len;
            if (expected_length < length) return utils.printErr(
                error.MissingArguments,
                "comperr: Too many items in array initializer list. Expected {}, received {} ({f})\n",
                .{
                    expected_length,
                    length,
                    try compiler.parser.getExprPos(.{ .array_instantiation = array }),
                },
                .red,
            ) else if (expected_length > length) return utils.printErr(
                error.TooManyArguments,
                "comperr: Missing items in array initializer list. Expected {}, received {} ({f})\n",
                .{
                    expected_length,
                    length,
                    try compiler.parser.getExprPos(.{ .array_instantiation = array }),
                },
                .red,
            );

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
        // TODO: this is bad
        var parent_expr_buf: std.ArrayList(u8) = .empty;
        try expressions.compile(compiler, &parent_expr_buf, member.parent, .{});
        const parent_type = compiler.getSymbolType(parent_expr_buf.items) catch return utils.printErr(
            error.UnknownSymbol,
            "comperr: Unknown symbol '{s}' at {f}\n",
            .{ parent_expr_buf.items, try compiler.parser.getExprPos(.{ .member = member }) },
            .red,
        );

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
                } else return utils.printErr(
                    error.UndeclaredProperty,
                    "comperr: '{f}' has no member '{s}' ({f})\n",
                    .{ parent_type, member.member_name, try compiler.parser.getExprPos(member.parent.*) },
                    .red,
                );
            },
            .reference => |reference| continue :b reference.inner.*,
            else => return utils.printErr(
                error.IllegalExpression,
                "comperr: Member expression on '{f}' is illegal\n",
                .{parent_type},
                .red,
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

    pub fn format(
        self: *const Self,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.*) {
            inline .@"struct", .@"enum", .@"union" => |compound| _ = try writer.write(compound.name),
            .optional => |optional| try writer.print("?{f}", .{optional}),
            .reference => |reference| try writer.print("&{s}{f}", .{
                if (reference.is_mut) "mut " else "",
                reference.inner,
            }),
            .array => |array| {
                _ = try writer.write("[");
                if (array.size) |size| try writer.print("{}", .{size});
                _ = try writer.write("]");
                try writer.print("{f}", .{array.inner});
            },
            .error_union => |error_union| {
                if (error_union.@"error") |err| try writer.print("{f}", .{err});
                try writer.print("!{f}", .{error_union.success});
            },
            .function => |function| {
                _ = try writer.write("fn (");
                for (function.params.items, 1..) |param, i| {
                    try writer.print("{f}", .{param});
                    if (i < function.params.items.len) _ = try writer.write(", ");
                }
                try writer.print(") {f}", .{function.return_type});
            },
            else => _ = try writer.write(@tagName(self.*)),
        }
    }
};

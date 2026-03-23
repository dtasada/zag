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

    @"typeof(nil)",
    @"typeof(undefined)",

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
                pub fn eql(lhs: Member, rhs: Member) bool {
                    return std.mem.eql(u8, lhs.name, rhs.name) and lhs.value == rhs.value;
                }
                pub fn clone(self: Member, alloc: std.mem.Allocator) !Member {
                    return .{ .name = try alloc.dupe(u8, self.name), .value = self.value };
                }
            } else struct {
                name: []const u8,
                type: Type,
                pub fn deinit(self: Member, alloc: std.mem.Allocator) void {
                    self.type.deinit(alloc);
                }
                pub fn eql(lhs: Member, rhs: Member) bool {
                    return std.mem.eql(u8, lhs.name, rhs.name) and lhs.type.eql(rhs.type);
                }
                pub fn clone(self: Member, alloc: std.mem.Allocator) !Member {
                    return .{ .name = try alloc.dupe(u8, self.name), .type = try self.type.clone(alloc) };
                }
            };

            name: []const u8,
            members: []const Member,
            symbols: []const Symbol,

            pub fn getMemberType(self: CompoundType(tag), name: []const u8) ?Type {
                for (self.members) |member| if (std.mem.eql(u8, member.name, name)) return member.type;
                for (self.symbols) |symbol| if (std.mem.eql(u8, symbol.name, name)) return symbol.type;

                return null;
            }
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

    pub fn fromAstPtr(alloc: std.mem.Allocator, t: *const ast.Type, c: *const Compiler) !*Type {
        const ret = try alloc.create(Type);
        errdefer alloc.destroy(ret);
        ret.* = try fromAst(alloc, t, c);
        return ret;
    }

    /// Caller owns memory
    pub fn fromAst(alloc: std.mem.Allocator, t: *const ast.Type, c: *const Compiler) Error!Type {
        return switch (t.*) {
            .symbol => |s| {
                const symbol = c.module.getSymbol(s.inner) orelse
                    return errors.unknownSymbol(s.inner, c.source_map[s.pos]);

                return switch (symbol.type) {
                    .type => symbol.value.?.type,
                    else => |received| errors.typeMismatch(.type, received, c.source_map[s.pos]),
                };
            },
            .optional => |opt| .{ .optional = try fromAstPtr(alloc, opt.inner, c) },
            inline .slice, .reference => |ref, tag| @unionInit(Type, @tagName(tag), .{
                .inner = try fromAstPtr(alloc, ref.inner, c),
                .is_mut = ref.is_mut,
            }),
            .array => |array| .{
                .array = .{
                    .inner = try fromAstPtr(alloc, array.inner, c),
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
                    .failure = try fromAstPtr(alloc, eu.failure, c),
                    .success = try fromAstPtr(alloc, eu.success, c),
                },
            },
            .function => |f| {
                var params: std.ArrayList(Type) = try .initCapacity(alloc, f.parameters.len);
                errdefer utils.deinitArrayList(Type, &params, alloc);
                for (f.parameters) |*param| params.appendAssumeCapacity(try fromAst(alloc, param, c));

                const return_type = try fromAstPtr(alloc, f.return_type, c);
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

    // User owns memory
    pub fn infer(alloc: std.mem.Allocator, expr: *const ast.Expression, c: *Compiler) !Type {
        return switch (expr.*) {
            .ident => |ident| if (c.module.getSymbol(ident.payload)) |symbol|
                try symbol.type.clone(alloc)
            else
                errors.unknownSymbol(ident.payload, c.source_map[ident.pos]),
            .string => try (Type{ .slice = .{ .is_mut = false, .inner = &.u8 } }).clone(alloc),
            .char => .u8,
            .int => .i32,
            .float => .f32,
            .@"if" => |cond| {
                const condition_t = try infer(alloc, cond.condition, c);
                defer condition_t.deinit(alloc);
                if (condition_t != .bool)
                    return errors.typeMismatch(.bool, condition_t, c.source_map[cond.condition.pos()]);

                const lhs_t = try infer(alloc, cond.body, c);
                const rhs_t = try infer(alloc, cond.@"else", c);

                const lhs_check_rhs = lhs_t.check(rhs_t);
                const rhs_check_lhs = rhs_t.check(lhs_t);
                if (!lhs_t.eql(rhs_t) and !lhs_check_rhs and !rhs_check_lhs) return errors.typesIncompatible(
                    lhs_t,
                    rhs_t,
                    c.source_map[cond.body.pos()],
                    c.source_map[cond.@"else".pos()],
                );

                if (lhs_check_rhs) {
                    defer rhs_t.deinit(alloc);
                    return lhs_t;
                } else {
                    defer lhs_t.deinit(alloc);
                    return rhs_t;
                }
            },
            .array_instantiation => |ai| {
                const inner = try fromAstPtr(alloc, &ai.type, c);
                errdefer inner.deinitPtr(alloc);

                const len: Value = try .eval(ai.length, c);
                errdefer len.deinit(alloc);

                if (len != .uint)
                    return errors.arrayLengthMustBeInteger(len.getType(), c.source_map[ai.length.pos()]);

                return .{
                    .array = .{
                        .inner = inner,
                        .len = len.uint,
                    },
                };
            },
            .assignment => .void,
            .binary => |binary| {
                const lhs: Type = try infer(alloc, binary.lhs, c);
                const rhs: Type = try infer(alloc, binary.rhs, c);
                defer rhs.deinit(alloc);
                if (!rhs.eql(lhs)) return utils.printErr(
                    error.TypeMismatch,
                    "comperr: Mismatched types in binary expression: '{f}' {s} '{f}' ({f}).\n",
                    .{ lhs, @tagName(binary.op), rhs, c.source_map[binary.pos] },
                );

                return switch (binary.op) {
                    .@"==", .@">", .@"<", .@">=", .@"<=", .@"!=", .@"and", .but, .@"or" => .bool,
                    else => lhs,
                };
            },
            .block => |block| {
                try c.module.pushScope(alloc);
                defer c.module.popScope(alloc);

                var found: ?struct { Type, usize } = null;
                for (block.payload) |statement| switch (statement) {
                    .variable_definition => |vd| try c.module.register(alloc, .{
                        .name = vd.variable_name,
                        .inner_name = vd.variable_name,
                        .type = if (vd.type == .inferred)
                            try infer(alloc, &vd.assigned_value, c)
                        else
                            try fromAst(alloc, &vd.type, c),
                        .binding = vd.binding,
                        .is_pub = vd.is_pub,
                        .free_inner_name = false,
                        .free_type = true,
                    }),
                    .block_eval => |*e| if (found) |f| {
                        return errors.doubleReturn(c.source_map[f[1]], c.source_map[e.pos()]);
                    } else {
                        found = .{ try infer(alloc, e, c), e.pos() };
                    },
                    else => {},
                };

                return if (found) |f| f[0] else .void;
            },
            .call => |call| {
                const callee_t = try infer(alloc, call.callee, c);
                defer callee_t.deinit(alloc);
                if (callee_t != .function)
                    return errors.expressionNotCallable(callee_t, c.source_map[call.callee.pos()]);

                return try callee_t.function.return_type.clone(alloc);
            },
            .comparison => |comp| {
                const left_t = try infer(alloc, comp.left, c);
                defer left_t.deinit(alloc);
                for (comp.comparisons) |i| {
                    const right_t = try infer(alloc, i.right, c);
                    defer right_t.deinit(alloc);
                    if (!right_t.eql(left_t))
                        return errors.typeMismatch(left_t, right_t, c.source_map[i.right.pos()]);
                }

                return .bool;
            },
            .dereference => |deref| {
                const inner_t = try infer(alloc, deref.parent, c);
                defer inner_t.deinit(alloc);
                if (inner_t != .reference) return errors.cannotDereference(inner_t, c.source_map[deref.pos]);
                return try inner_t.reference.inner.clone(alloc);
            },
            .index => |index| {
                const inner_t = try infer(alloc, index.lhs, c);
                defer inner_t.deinit(alloc);
                if (inner_t != .slice and inner_t != .array)
                    return errors.cannotDereference(inner_t, c.source_map[index.pos]);
                return switch (inner_t) {
                    inline .slice, .array => |t| try t.inner.clone(alloc),
                    else => unreachable,
                };
            },
            .slice => |slice| {
                const lhs_t = try infer(alloc, slice.lhs, c);
                defer lhs_t.deinit(alloc);
                if (lhs_t != .slice and lhs_t != .array)
                    return errors.cannotSlice(lhs_t, c.source_map[slice.pos]);
                return .{
                    .slice = .{
                        .inner = switch (lhs_t) {
                            inline .slice, .array => |s| try s.inner.clonePtr(alloc),
                            else => unreachable,
                        },
                        .is_mut = c.module.getExpressionMutability(slice.lhs) catch |err| switch (err) {
                            error.UnknownSymbol => return errors.unknownSymbol(
                                slice.lhs.ident.payload,
                                c.source_map[slice.lhs.ident.pos],
                            ),
                            else => return err,
                        },
                    },
                };
            },
            .member => |member| {
                const t = try infer(alloc, member.parent, c);
                defer t.deinit(alloc);
                return switch (t) {
                    inline .@"struct", .@"union" => |ct| ct.getMemberType(member.member_name) orelse
                        errors.unknownMember(t, member.member_name, c.source_map[member.pos]),
                    .@"enum" => .usize,
                    .slice => |slc| if (std.mem.eql(u8, member.member_name, "ptr")) .{
                        .reference = .{
                            .inner = try slc.inner.clonePtr(alloc),
                            .is_mut = slc.is_mut,
                        },
                    } else if (std.mem.eql(u8, member.member_name, "len"))
                        .usize
                    else
                        errors.badMemberAccessSlice(t, member.member_name, c.source_map[member.pos]),
                    else => |parent_t| errors.badMemberAccess(parent_t, member.member_name, c.source_map[member.pos]),
                };
            },
            .prefix => |prefix| {
                const rhs_t = try infer(alloc, prefix.rhs, c);
                if (!rhs_t.isNumeric() and rhs_t != .bool or
                    rhs_t.isNumeric() and prefix.op == .@"!" or
                    rhs_t == .bool and prefix.op == .@"-")
                    return errors.illegalPrefixOp(rhs_t, prefix.op, c.source_map[prefix.pos]);
                return rhs_t;
            },
            .reference => |ref| {
                const rhs_t = try alloc.create(Type);
                rhs_t.* = try infer(alloc, ref.inner, c);
                if (ref.is_mut and !try c.module.getExpressionMutability(ref.inner))
                    return errors.mutRefOfConst(c.source_map[ref.pos]);
                return .{
                    .reference = .{
                        .inner = rhs_t,
                        .is_mut = ref.is_mut,
                    },
                };
            },
            .struct_instantiation => |si| {
                const st = try infer(alloc, si.type_expr, c);
                defer st.deinit(alloc);
                if (st != .type) return errors.exprIsNotStruct(st, c.source_map[si.pos]);

                const symbol = c.module.getSymbol(si.type_expr.ident.payload).?;
                return try symbol.value.?.type.clone(alloc);
            },
            .type => |t| fromAst(alloc, &t.payload, c),
            else => unreachable,
        };
    }

    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .i8,
            .i16,
            .i32,
            .i64,
            .isize,
            .u8,
            .u16,
            .u32,
            .u64,
            .usize,
            .f32,
            .f64,
            .c_char,
            .c_short,
            .c_int,
            .c_long,
            .c_uchar,
            .c_ushort,
            .c_uint,
            .c_ulong,
            .c_float,
            .c_double,
            .reference,
            => true,
            else => false,
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

    pub fn check(received: Type, expected: Type) bool {
        if (received.eql(expected)) return true;

        const fallback = switch (expected) {
            .optional => |inner| received.check(inner.*),
            .error_union => |error_union| received.check(error_union.success.*) or
                received.check(error_union.failure.*),
            else => expected.isNumeric() and received.isNumeric() or received.eql(expected),
        };

        return switch (received) {
            .@"typeof(undefined)" => true,
            .@"typeof(nil)" => true,
            .optional => |ro| switch (expected) {
                .optional => |eo| ro.check(eo.*),
                else => fallback,
            },
            .slice => |rs| switch (expected) {
                .reference => |er| rs.inner.* == .u8 and er.inner.* == .c_char,
                .slice => |es| rs.inner.check(es.inner.*) and (rs.is_mut or !es.is_mut),
                else => fallback,
            },
            .reference => |received_ref| switch (expected) {
                .optional => |expected_opt| received.check(expected_opt.*),
                .reference => |expected_ref| received_ref.inner.check(expected_ref.inner.*) and
                    (received_ref.is_mut or !expected_ref.is_mut) or
                    expected_ref.inner.* == .void or received_ref.inner.* == .void,
                .slice => |slc| received_ref.inner.* == .array and
                    received_ref.inner.array.inner.check(slc.inner.*),
                else => fallback,
            },
            else => fallback,
        };
    }

    pub fn eql(lhs: Type, rhs: Type) bool {
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

        return switch (lhs) {
            .optional => lhs.optional.eql(rhs.optional.*),
            .reference => lhs.reference.inner.eql(rhs.reference.inner.*) and
                lhs.reference.is_mut == rhs.reference.is_mut,
            .slice => lhs.slice.inner.eql(rhs.slice.inner.*) and lhs.slice.is_mut == rhs.slice.is_mut,
            .array => lhs.array.inner.eql(rhs.array.inner.*) and lhs.array.len == rhs.array.len,
            .error_union => lhs.error_union.success.eql(rhs.error_union.success.*) and
                lhs.error_union.failure.eql(rhs.error_union.failure.*),
            .function => lhs.function.return_type.eql(rhs.function.return_type.*) and
                lhs.function.parameters.len == rhs.function.parameters.len and b: {
                for (0..lhs.function.parameters.len) |i| {
                    if (!lhs.function.parameters[i].eql(rhs.function.parameters[i]))
                        break :b false;
                }
                break :b true;
            },
            inline .@"struct", .@"enum", .@"union" => |ct, t| {
                const a = ct;
                const b = @field(rhs, @tagName(t));

                if (!std.mem.eql(u8, a.name, b.name)) return false;
                if (a.members.len != b.members.len) return false;
                if (a.symbols.len != b.symbols.len) return false;

                for (0..a.members.len) |i| {
                    if (!a.members[i].eql(b.members[i])) return false;
                }
                for (0..a.symbols.len) |i| {
                    if (!a.symbols[i].eql(b.symbols[i])) return false;
                }

                return true;
            },
            .module => std.mem.eql(u8, lhs.module.name, rhs.module.name),
            else => true,
        };
    }

    pub fn clonePtr(self: Type, alloc: std.mem.Allocator) std.mem.Allocator.Error!*Type {
        const ret = try alloc.create(Type);
        errdefer alloc.destroy(ret);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Type, alloc: std.mem.Allocator) std.mem.Allocator.Error!Type {
        return switch (self) {
            .optional => |opt| .{ .optional = try opt.clonePtr(alloc) },
            inline .reference, .slice => |ref, tag| @unionInit(Type, @tagName(tag), .{
                .inner = try ref.inner.clonePtr(alloc),
                .is_mut = ref.is_mut,
            }),
            .array => |array| .{
                .array = .{
                    .inner = try array.inner.clonePtr(alloc),
                    .len = array.len,
                },
            },
            .error_union => |eu| .{
                .error_union = .{
                    .failure = try eu.failure.clonePtr(alloc),
                    .success = try eu.success.clonePtr(alloc),
                },
            },
            .function => |f| .{
                .function = .{
                    .parameters = try utils.cloneSlice(Type, f.parameters, alloc),
                    .return_type = try f.return_type.clonePtr(alloc),
                },
            },
            inline .@"struct", .@"enum", .@"union" => |ct, tag| @unionInit(Type, @tagName(tag), .{
                .name = try alloc.dupe(u8, ct.name),
                .members = try utils.cloneSlice(@TypeOf(ct).Member, ct.members, alloc),
                .symbols = try utils.cloneSlice(Symbol, ct.symbols, alloc),
            }),
            else => self,
        };
    }
};

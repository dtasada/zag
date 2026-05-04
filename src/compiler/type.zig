const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const errors = @import("errors.zig");
const compiler = @import("compiler.zig");
const generics = @import("generics.zig");
const statements = @import("statements.zig");

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
    range,
    any,

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

    module: *Module,
    template: Template,

    pub fn getMangledName(
        alloc: std.mem.Allocator,
        io: std.Io,
        template_name: []const u8,
        args: []const ast.Expression,
        c: *Compiler,
        m: *const Module,
    ) Error![]const u8 {
        var mangled_name: std.ArrayList(u8) = .empty;
        errdefer mangled_name.deinit(alloc);

        try mangled_name.appendSlice(alloc, template_name);
        try mangled_name.append(alloc, '_');

        for (args) |arg| {
            const val: Value = try .eval(alloc, io, &arg, c, m);
            defer val.deinit(alloc);

            switch (val) {
                .uint => |u| try mangled_name.print(alloc, "u{}", .{u}),
                .int => |i| try mangled_name.print(alloc, "i{}", .{i}),
                .float => |f| try mangled_name.print(alloc, "f{}", .{@as(u64, @bitCast(f))}),
                .bool => |b| try mangled_name.appendSlice(alloc, if (b) "true" else "false"),
                .type => |t| try mangled_name.print(alloc, "{x}", .{t.hash()}),
                else => try mangled_name.appendSlice(alloc, "unknown"),
            }
            try mangled_name.append(alloc, '_');
        }

        return mangled_name.toOwnedSlice(alloc);
    }

    pub fn instantiate(
        self: Type,
        alloc: std.mem.Allocator,
        io: std.Io,
        args: []const ast.Expression,
        c: *Compiler,
        m: *const Module,
        pos: usize,
    ) Error!Type {
        if (self != .template) return errors.expressionNotGeneric(io, m.source_map[pos]);

        const template = self.template;
        const template_name = switch (template.kind) {
            .builtin_cast => "cast",
            .builtin_sizeof => "sizeof",
            inline else => |d| d.name,
        };

        const mangled_name = try getMangledName(alloc, io, template_name, args, c, m);
        defer alloc.free(mangled_name);

        if (c.module.instantiations.get(mangled_name)) |t| return try t.clone(alloc);

        const result_t: Type = switch (template.kind) {
            .builtin_cast => {
                if (args.len != 1) return error.TypeMismatch;
                const target_t = try fromAstPtr(alloc, io, &args[0].type.payload, c, m);
                errdefer target_t.deinitPtr(alloc);

                const params = try alloc.alloc(Type, 1);
                params[0] = .any;

                return .{
                    .function = .{
                        .parameters = params,
                        .return_type = target_t,
                    },
                };
            },
            .builtin_sizeof => {
                if (args.len != 1) return error.TypeMismatch;
                return .{
                    .function = .{
                        .parameters = &.{},
                        .return_type = try Type.clonePtr(.usize, alloc),
                    },
                };
            },
            .function_definition => |fd| b: {
                var new_fd = try fd.clone(alloc);
                defer new_fd.deinit(alloc);

                var map: std.StringHashMap(ast.Expression) = .init(alloc);
                defer map.deinit();

                var arg_i: usize = 0;
                for (fd.generic_parameters) |group| {
                    for (group.names) |name| {
                        if (arg_i >= args.len) return error.NotEnoughGenericArguments;
                        try map.put(name, args[arg_i]);
                        arg_i += 1;
                    }
                }

                try generics.mapGenerics(alloc, map, &new_fd);

                utils.deinitSlice(ast.ParameterGroup, new_fd.generic_parameters, alloc);
                new_fd.generic_parameters = &.{};

                const old_name = new_fd.name;
                new_fd.name = try alloc.dupe(u8, mangled_name);
                defer {
                    alloc.free(new_fd.name);
                    new_fd.name = old_name;
                }

                const tls: ast.TopLevelStatement = .{ .function_definition = new_fd };
                try statements.compileTopLevel(alloc, io, tls, c, template.module);

                const symbol = c.module.getSymbol(mangled_name) orelse return error.InstantiationFailed;
                break :b try symbol.type.clone(alloc);
            },
            inline .struct_declaration, .union_declaration => |d, tag| b: {
                var new_ud = try d.clone(alloc);
                defer new_ud.deinit(alloc);

                var map: std.StringHashMap(ast.Expression) = .init(alloc);
                defer map.deinit();

                var arg_i: usize = 0;
                for (d.generic_types) |group| {
                    for (group.names) |name| {
                        if (arg_i >= args.len) return error.NotEnoughGenericArguments;
                        try map.put(name, args[arg_i]);
                        arg_i += 1;
                    }
                }

                try generics.mapGenerics(alloc, map, &new_ud);

                utils.deinitSlice(ast.ParameterGroup, new_ud.generic_types, alloc);
                new_ud.generic_types = &.{};

                const old_name = new_ud.name;
                new_ud.name = try alloc.dupe(u8, mangled_name);
                defer {
                    alloc.free(new_ud.name);
                    new_ud.name = old_name;
                }

                const tls = @unionInit(ast.TopLevelStatement, @tagName(tag), new_ud);
                try statements.compileTopLevel(alloc, io, tls, c, template.module);

                const symbol = c.module.getSymbol(mangled_name) orelse return error.InstantiationFailed;
                break :b try symbol.value.?.type.clone(alloc);
            },
        };

        try c.module.instantiations.put(try alloc.dupe(u8, mangled_name), try result_t.clone(alloc));
        return result_t;
    }

    pub const Template = struct {
        pub const Kind = union(enum) {
            function_definition: ast.TopLevelStatement.FunctionDefinition,
            struct_declaration: ast.TopLevelStatement.StructDeclaration,
            union_declaration: ast.TopLevelStatement.UnionDeclaration,
            builtin_cast,
            builtin_sizeof,
        };

        module: *const Module,
        kind: Kind,
    };
    const Reference = struct { inner: *const Type, is_mut: bool };
    const Slice = struct { inner: *const Type, is_mut: bool };
    const Array = struct { inner: *const Type, len: usize };
    const ErrorUnion = struct { failure: *const Type, success: *const Type };

    pub const Struct = CompoundType(.@"struct");
    pub const Enum = CompoundType(.@"enum");
    pub const Union = CompoundType(.@"union");

    pub fn CompoundType(tag: utils.CompoundTypeTag) type {
        return struct {
            pub const Member = if (tag == .@"enum") struct {
                name: []const u8,
                inner_name: []const u8,
                value: usize,
                pub fn deinit(self: Member, alloc: std.mem.Allocator) void {
                    alloc.free(self.name);
                    alloc.free(self.inner_name);
                }
                pub fn eql(lhs: Member, rhs: Member) bool {
                    return std.mem.eql(u8, lhs.name, rhs.name) and
                        std.mem.eql(u8, lhs.inner_name, rhs.inner_name) and
                        lhs.value == rhs.value;
                }
                pub fn clone(self: Member, alloc: std.mem.Allocator) Error!Member {
                    const name = try alloc.dupe(u8, self.name);
                    errdefer alloc.free(name);

                    const inner_name = try alloc.dupe(u8, self.inner_name);
                    errdefer alloc.free(inner_name);

                    return .{
                        .name = name,
                        .inner_name = inner_name,
                        .value = self.value,
                    };
                }
            } else struct {
                name: []const u8,
                inner_name: []const u8,
                type: Type,
                pub fn deinit(self: Member, alloc: std.mem.Allocator) void {
                    alloc.free(self.name);
                    alloc.free(self.inner_name);
                    self.type.deinit(alloc);
                }
                pub fn eql(lhs: Member, rhs: Member) bool {
                    return std.mem.eql(u8, lhs.name, rhs.name) and
                        std.mem.eql(u8, lhs.inner_name, rhs.inner_name) and
                        lhs.type.eql(rhs.type);
                }
                pub fn clone(self: Member, alloc: std.mem.Allocator) Error!Member {
                    const name = try alloc.dupe(u8, self.name);
                    errdefer alloc.free(name);

                    const inner_name = try alloc.dupe(u8, self.inner_name);
                    errdefer alloc.free(inner_name);

                    const t = try self.type.clone(alloc);
                    errdefer t.deinit(alloc);

                    return .{
                        .name = name,
                        .inner_name = inner_name,
                        .type = t,
                    };
                }
            };

            name: []const u8,
            members: []const Member,
            symbols: []const Symbol,
            tag_type: ?*const Type,

            pub fn getMemberType(self: CompoundType(tag), name: []const u8) ?Type {
                for (self.members) |member| if (std.mem.eql(u8, member.name, name)) return member.type;
                for (self.symbols) |symbol| if (std.mem.eql(u8, symbol.name, name)) return symbol.type;

                return null;
            }

            pub fn hasMember(self: CompoundType(tag), name: []const u8) bool {
                for (self.members) |member| if (std.mem.eql(u8, member.name, name)) return true;
                for (self.symbols) |symbol| if (std.mem.eql(u8, symbol.name, name)) return true;

                return false;
            }
        };
    }
    pub const Function = struct {
        parameters: []const Type,
        return_type: *Type,

        pub fn deinit(self: Function, alloc: std.mem.Allocator) void {
            utils.deinitSlice(Type, self.parameters, alloc);
            self.return_type.deinitPtr(alloc);
        }
    };

    pub fn fromAstPtr(
        alloc: std.mem.Allocator,
        io: std.Io,
        t: *const ast.Type,
        c: *Compiler,
        m: *const Module,
    ) Error!*Type {
        const ret = try alloc.create(Type);
        errdefer alloc.destroy(ret);
        ret.* = try fromAst(alloc, io, t, c, m);
        return ret;
    }

    /// Caller owns memory
    pub fn fromAst(
        alloc: std.mem.Allocator,
        io: std.Io,
        t: *const ast.Type,
        c: *Compiler,
        module: *const Module,
    ) Error!Type {
        return switch (t.*) {
            .symbol => |s| {
                const symbol = module.getSymbol(s.inner) orelse
                    return errors.unknownSymbol(io, s.inner, module.source_map[s.pos]);

                return switch (symbol.type) {
                    .type => try symbol.value.?.type.clone(alloc),
                    .template => try symbol.type.clone(alloc),
                    .module => try symbol.type.clone(alloc),
                    else => |received| errors.typeMismatch(io, .type, received, module.source_map[s.pos]),
                };
            },
            .optional => |opt| .{ .optional = try fromAstPtr(alloc, io, opt.inner, c, module) },
            inline .slice, .reference => |ref, tag| @unionInit(Type, @tagName(tag), .{
                .inner = try fromAstPtr(alloc, io, ref.inner, c, module),
                .is_mut = ref.is_mut,
            }),
            .array => |array| .{
                .array = .{
                    .inner = try fromAstPtr(alloc, io, array.inner, c, module),
                    .len = switch (try Value.eval(alloc, io, array.size, c, module)) {
                        .uint => |uint| uint,
                        else => |val| return errors.arrayLengthMustBeInteger(
                            io,
                            val.getType(),
                            module.source_map[array.size.pos()],
                        ),
                    },
                },
            },
            .error_union => |eu| .{
                .error_union = .{
                    .failure = try fromAstPtr(alloc, io, eu.failure, c, module),
                    .success = try fromAstPtr(alloc, io, eu.success, c, module),
                },
            },
            .function => |f| {
                var params: std.ArrayList(Type) = try .initCapacity(alloc, f.parameters.len);
                errdefer utils.deinitArrayList(Type, &params, alloc);

                for (f.parameters) |*param| params.appendAssumeCapacity(try fromAst(alloc, io, param, c, module));

                const return_type = try fromAstPtr(alloc, io, f.return_type, c, module);
                errdefer return_type.deinitPtr(alloc);

                return .{
                    .function = .{
                        .parameters = try params.toOwnedSlice(alloc),
                        .return_type = return_type,
                    },
                };
            },
            .generic => |g| {
                const lhs_t = try fromAst(alloc, io, g.lhs, c, module);
                defer lhs_t.deinit(alloc);

                return try lhs_t.instantiate(alloc, io, g.arguments, c, module, g.pos);
            },
            .variadic => .variadic,
            .member => |m| {
                const parent = try fromAst(alloc, io, m.parent, c, module);
                defer parent.deinit(alloc);

                switch (parent) {
                    .@"struct" => |s| {
                        for (s.symbols) |symbol| {
                            if (std.mem.eql(u8, symbol.name, m.member_name)) {
                                if (symbol.type == .type) {
                                    return try symbol.value.?.type.clone(alloc);
                                }
                            }
                        }
                        return errors.unknownMember(io, parent, m.member_name, module.source_map[m.pos]);
                    },
                    .module => |mod| {
                        if (mod.getSymbol(m.member_name)) |symbol| {
                            if (symbol.type == .type) return try symbol.value.?.type.clone(alloc);
                            if (symbol.type == .template) return try symbol.type.clone(alloc);
                        }
                        return errors.unknownMember(io, parent, m.member_name, module.source_map[m.pos]);
                    },
                    else => return errors.badMemberAccess(io, parent, m.member_name, module.source_map[m.pos]),
                }
            },
        };
    }

    // Caller owns memory
    pub fn infer(
        alloc: std.mem.Allocator,
        io: std.Io,
        expr: *const ast.Expression,
        c: *Compiler,
        m: *const Module,
    ) Error!Type {
        return switch (expr.*) {
            .ident => |ident| if (c.module.getSymbol(ident.payload)) |symbol|
                try symbol.type.clone(alloc)
            else
                errors.unknownSymbol(io, ident.payload, m.source_map[ident.pos]),
            .string => try (Type{ .slice = .{ .is_mut = false, .inner = &.u8 } }).clone(alloc),
            .char => .u8,
            .int => .i32,
            .float => .f32,
            .@"if" => |cond| {
                const condition_t = try infer(alloc, io, cond.condition, c, m);
                defer condition_t.deinit(alloc);
                if (condition_t != .bool)
                    return errors.typeMismatch(io, .bool, condition_t, m.source_map[cond.condition.pos()]);

                const lhs_t = try infer(alloc, io, cond.body, c, m);
                errdefer lhs_t.deinit(alloc);

                const rhs_t = try infer(alloc, io, cond.@"else", c, m);
                errdefer rhs_t.deinit(alloc);

                const lhs_check_rhs = lhs_t.check(rhs_t);
                const rhs_check_lhs = rhs_t.check(lhs_t);
                if (!lhs_t.eql(rhs_t) and !lhs_check_rhs and !rhs_check_lhs) return errors.typesIncompatible(
                    io,
                    lhs_t,
                    rhs_t,
                    m.source_map[cond.body.pos()],
                    m.source_map[cond.@"else".pos()],
                );

                if (lhs_check_rhs) {
                    rhs_t.deinit(alloc);
                    return lhs_t;
                } else {
                    lhs_t.deinit(alloc);
                    return rhs_t;
                }
            },
            .array_instantiation => |ai| {
                const inner = try fromAstPtr(alloc, io, &ai.type, c, m);
                errdefer inner.deinitPtr(alloc);

                const len: Value = if (ai.length.* == .ident and std.mem.eql(u8, ai.length.ident.payload, "_"))
                    .{ .uint = ai.contents.len }
                else
                    try .eval(alloc, io, ai.length, c, m);
                errdefer len.deinit(alloc);

                if (len != .uint)
                    return errors.arrayLengthMustBeInteger(io, len.getType(), m.source_map[ai.length.pos()]);

                if (len.uint != ai.contents.len)
                    return errors.arrayInstantiationSizeMismatch(io, len.uint, ai.contents.len, m.source_map[ai.pos]);

                return .{
                    .array = .{
                        .inner = inner,
                        .len = len.uint,
                    },
                };
            },
            .assignment => .void,
            .binary => |binary| {
                const lhs: Type = try infer(alloc, io, binary.lhs, c, m);
                const rhs: Type = try infer(alloc, io, binary.rhs, c, m);
                defer rhs.deinit(alloc);
                if (!rhs.check(lhs)) return utils.printErr( // i feel like this should be eql instead of check but...
                    io,
                    error.TypeMismatch,
                    "comperr: Mismatched types in binary expression: '{f}' {s} '{f}' ({f}).\n",
                    .{ lhs, @tagName(binary.op), rhs, m.source_map[binary.pos] },
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
                        .type = if (vd.type) |*t|
                            try fromAst(alloc, io, t, c, m)
                        else
                            try infer(alloc, io, &vd.assigned_value, c, m),
                        .binding = vd.binding,
                        .is_pub = vd.is_pub,
                        .free_name = false,
                        .free_inner_name = false,
                        .free_type = true,
                    }),
                    .block_eval => |*e| if (found) |f| {
                        return errors.doubleReturn(io, m.source_map[f[1]], m.source_map[e.pos()]);
                    } else {
                        found = .{ try infer(alloc, io, e, c, m), e.pos() };
                    },
                    else => {},
                };

                return if (found) |f| f[0] else .void;
            },
            .call => |call| {
                const callee_t = try infer(alloc, io, call.callee, c, m);
                defer callee_t.deinit(alloc);

                if (callee_t != .function)
                    return errors.expressionNotCallable(io, callee_t, m.source_map[call.callee.pos()]);

                return try callee_t.function.return_type.clone(alloc);
            },
            .comparison => |comp| {
                const left_t = try infer(alloc, io, comp.left, c, m);
                defer left_t.deinit(alloc);
                for (comp.comparisons) |i| {
                    const right_t = try infer(alloc, io, i.right, c, m);
                    defer right_t.deinit(alloc);
                    if (!right_t.eql(left_t))
                        return errors.typeMismatch(io, left_t, right_t, m.source_map[i.right.pos()]);
                }

                return .bool;
            },
            .dereference => |deref| {
                const inner_t = try infer(alloc, io, deref.parent, c, m);
                defer inner_t.deinit(alloc);
                if (inner_t != .reference) return errors.cannotDereference(io, inner_t, m.source_map[deref.pos]);
                return try inner_t.reference.inner.clone(alloc);
            },
            .index => |index| {
                const inner_t = try infer(alloc, io, index.lhs, c, m);
                defer inner_t.deinit(alloc);
                if (inner_t != .slice and inner_t != .array)
                    return errors.cannotDereference(io, inner_t, m.source_map[index.pos]);
                return switch (inner_t) {
                    inline .slice, .array => |t| try t.inner.clone(alloc),
                    else => unreachable,
                };
            },
            .slice => |slice| {
                const lhs_t = try infer(alloc, io, slice.lhs, c, m);
                defer lhs_t.deinit(alloc);
                if (lhs_t != .slice and lhs_t != .array)
                    return errors.cannotSlice(io, lhs_t, m.source_map[slice.pos]);
                return .{
                    .slice = .{
                        .inner = switch (lhs_t) {
                            inline .slice, .array => |s| try s.inner.clonePtr(alloc),
                            else => unreachable,
                        },
                        .is_mut = c.module.getExpressionMutability(alloc, io, slice.lhs, c) catch |err| switch (err) {
                            error.UnknownSymbol => return errors.unknownSymbol(
                                io,
                                slice.lhs.ident.payload,
                                m.source_map[slice.lhs.ident.pos],
                            ),
                            else => return err,
                        },
                    },
                };
            },
            .member => |member| {
                var t = try infer(alloc, io, member.parent, c, m);
                while (t == .reference) {
                    const next_t = try t.reference.inner.clone(alloc);
                    t.deinit(alloc);
                    t = next_t;
                }
                defer t.deinit(alloc);

                return switch (t) {
                    .@"struct" => |ct| {
                        const member_t = ct.getMemberType(member.member_name) orelse
                            return errors.unknownMember(io, t, member.member_name, m.source_map[member.pos]);
                        return try member_t.clone(alloc);
                    },
                    .@"union" => |ct| {
                        const member_t = ct.getMemberType(member.member_name) orelse
                            return errors.unknownMember(io, t, member.member_name, m.source_map[member.pos]);
                        return try member_t.clone(alloc);
                    },
                    .@"enum" => .usize,
                    .module => |mod| if (mod.getSymbol(member.member_name)) |symbol|
                        try symbol.type.clone(alloc)
                    else
                        errors.unknownMember(io, t, member.member_name, m.source_map[member.pos]),
                    .slice => |slc| if (std.mem.eql(u8, member.member_name, "ptr")) .{
                        .reference = .{
                            .inner = try slc.inner.clonePtr(alloc),
                            .is_mut = slc.is_mut,
                        },
                    } else if (std.mem.eql(u8, member.member_name, "len"))
                        .usize
                    else
                        errors.badMemberAccessSlice(io, t, member.member_name, m.source_map[member.pos]),
                    else => |parent_t| errors.badMemberAccess(io, parent_t, member.member_name, m.source_map[member.pos]),
                };
            },
            .prefix => |prefix| {
                const rhs_t = try infer(alloc, io, prefix.rhs, c, m);
                if (!rhs_t.isNumeric() and rhs_t != .bool or
                    rhs_t.isNumeric() and prefix.op == .@"!" or
                    rhs_t == .bool and prefix.op == .@"-")
                    return errors.illegalPrefixOp(io, rhs_t, prefix.op, m.source_map[prefix.pos]);
                return rhs_t;
            },
            .reference => |ref| {
                const rhs_t = try alloc.create(Type);
                rhs_t.* = try infer(alloc, io, ref.inner, c, m);
                if (ref.is_mut and !try c.module.getExpressionMutability(alloc, io, ref.inner, c))
                    return errors.mutRefOfConst(io, m.source_map[ref.pos]);
                return .{
                    .reference = .{
                        .inner = rhs_t,
                        .is_mut = ref.is_mut,
                    },
                };
            },
            .struct_instantiation => |si| {
                const st = try infer(alloc, io, si.type_expr, c, m);
                defer st.deinit(alloc);
                if (st != .type) return errors.exprIsNotStruct(io, st, m.source_map[si.pos]);

                const symbol = c.module.getSymbolFromExpression(alloc, io, si.type_expr, c) orelse
                    return errors.exprIsNotStruct(io, st, m.source_map[si.pos]);

                return try symbol.value.?.type.clone(alloc);
            },
            .type => .type,
            .generic => |generic| {
                const lhs_t = try infer(alloc, io, generic.lhs, c, m);
                defer lhs_t.deinit(alloc);

                const t = try lhs_t.instantiate(alloc, io, generic.arguments, c, m, generic.pos);
                if (lhs_t == .template and
                    lhs_t.template.kind != .function_definition and
                    lhs_t.template.kind != .builtin_cast and
                    lhs_t.template.kind != .builtin_sizeof)
                {
                    t.deinit(alloc);
                    return .type;
                }
                return t;
            },
            inline else => |_, tag| std.debug.panic("unreachable with {s}\n", .{@tagName(tag)}),
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

    pub fn isInteger(self: Type) bool {
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
            .c_char,
            .c_short,
            .c_int,
            .c_long,
            .c_uchar,
            .c_ushort,
            .c_uint,
            .c_ulong,
            => true,
            else => false,
        };
    }

    pub fn smallestIntegerFor(n: usize) Type {
        const log2 = std.math.log2_int_ceil(usize, n);
        return if (log2 <= 8)
            .u8
        else if (log2 <= 16)
            .u16
        else if (log2 <= 32)
            .u32
        else if (log2 <= 64)
            .u64
        else
            unreachable;
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
                alloc.free(ct.name);
                utils.deinitSlice(@TypeOf(ct).Member, ct.members, alloc);
                utils.deinitSlice(Symbol, ct.symbols, alloc);
                if (ct.tag_type) |tt| tt.deinitPtr(alloc);
            },
            .template => |template| switch (template.kind) {
                .function_definition => |fd| fd.deinit(alloc),
                .struct_declaration => |sd| sd.deinit(alloc),
                .union_declaration => |ud| ud.deinit(alloc),
                .builtin_cast, .builtin_sizeof => {},
            },
            .module => {},
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
                    if (tag == .@"enum") {
                        h.update(std.mem.asBytes(&member.value));
                    } else {
                        const m_hash = member.type.hash();
                        h.update(std.mem.asBytes(&m_hash));
                    }
                }
            },
            .template => |t| switch (t.kind) {
                inline .function_definition, .struct_declaration, .union_declaration => |d| h.update(d.name),
                .builtin_cast => h.update("cast"),
                .builtin_sizeof => h.update("sizeof"),
            },
            else => {},
        }

        return h.final();
    }

    pub fn check(received: Type, expected: Type) bool {
        if (expected == .any) return true;
        if (received.eql(expected)) return true;

        const fallback = switch (expected) {
            .variadic => true,
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
            .template => |t| {
                const rt = rhs.template;
                if (std.meta.activeTag(t.kind) != std.meta.activeTag(rt.kind)) return false;
                return switch (t.kind) {
                    .function_definition => |fd| std.mem.eql(u8, fd.name, rt.kind.function_definition.name),
                    .struct_declaration => |sd| std.mem.eql(u8, sd.name, rt.kind.struct_declaration.name),
                    .union_declaration => |ud| std.mem.eql(u8, ud.name, rt.kind.union_declaration.name),
                    .builtin_cast, .builtin_sizeof => true,
                };
            },
            .module => std.mem.eql(u8, lhs.module.name, rhs.module.name),
            else => true,
        };
    }

    pub fn clonePtr(self: Type, alloc: std.mem.Allocator) Error!*Type {
        const ret = try alloc.create(Type);
        errdefer alloc.destroy(ret);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Type, alloc: std.mem.Allocator) Error!Type {
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
            .error_union => |eu| {
                const failure = try eu.failure.clonePtr(alloc);
                errdefer failure.deinitPtr(alloc);

                const success = try eu.success.clonePtr(alloc);
                errdefer success.deinitPtr(alloc);

                return .{
                    .error_union = .{
                        .failure = failure,
                        .success = success,
                    },
                };
            },
            .function => |f| {
                const parameters = try utils.cloneSlice(Type, f.parameters, alloc);
                errdefer utils.deinitSlice(Type, parameters, alloc);

                const return_type = try f.return_type.clonePtr(alloc);
                errdefer return_type.deinitPtr(alloc);

                return .{
                    .function = .{
                        .parameters = parameters,
                        .return_type = return_type,
                    },
                };
            },
            inline .@"struct", .@"enum", .@"union" => |ct, tag| {
                const name = try alloc.dupe(u8, ct.name);
                errdefer alloc.free(name);

                const members = try utils.cloneSlice(@TypeOf(ct).Member, ct.members, alloc);
                errdefer utils.deinitSlice(@TypeOf(ct).Member, members, alloc);

                const symbols = try utils.cloneSlice(Symbol, ct.symbols, alloc);
                errdefer utils.deinitSlice(Symbol, symbols, alloc);

                const tag_type = if (ct.tag_type) |tt| try tt.clonePtr(alloc) else null;
                errdefer if (tag_type) |tt| tt.deinitPtr(alloc);

                return @unionInit(Type, @tagName(tag), .{
                    .name = name,
                    .members = members,
                    .symbols = symbols,
                    .tag_type = tag_type,
                });
            },
            .template => |template| return .{
                .template = .{
                    .module = template.module,
                    .kind = switch (template.kind) {
                        .function_definition => |fd| .{ .function_definition = try fd.clone(alloc) },
                        .struct_declaration => |sd| .{ .struct_declaration = try sd.clone(alloc) },
                        .union_declaration => |ud| .{ .union_declaration = try ud.clone(alloc) },
                        .builtin_cast => .builtin_cast,
                        .builtin_sizeof => .builtin_sizeof,
                    },
                },
            },
            else => self,
        };
    }
};

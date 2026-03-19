const std = @import("std");
const ast = @import("ast");
const utils = @import("utils");
const Compiler = @import("../Compiler.zig");
const Module = @import("../Module.zig");
const Value = @import("../Value.zig").Value;
const Type = @import("Type.zig").Type;
const errors = @import("../errors.zig");
const CompilerError = errors.CompilerError;

pub const Struct = CompoundType(.@"struct");
pub const Union = CompoundType(.@"union");
pub const Enum = CompoundType(.@"enum");

pub const ErrorUnion = struct {
    success: *const Type,
    failure: *const Type,
};

pub const Slice = Reference;

pub const Array = struct {
    inner: *const Type,
    size: usize,
};

pub const Reference = struct {
    inner: *const Type,
    is_mut: bool,
};

pub const Function = struct {
    pub const Param = struct {
        name: []const u8,
        type: Type,

        pub fn clone(self: Param, alloc: std.mem.Allocator) !Param {
            return .{ .name = self.name, .type = try self.type.clone(alloc) };
        }
    };

    name: []const u8,
    inner_name: []const u8,
    params: []const Param,
    generic_params: []const Param,
    return_type: *const Type,
    definition: ?*const ast.Statement.FunctionDefinition = null,
    module: Module,
    generic_instantiation: ?GenericInstantiation = null,
    is_bind: bool = false,

    pub fn fromMethod(method: Method, module: Module) Type {
        return .{
            .function = .{
                .name = method.inner_name,
                .inner_name = method.inner_name,
                .params = method.params,
                .generic_params = method.generic_params,
                .return_type = method.return_type,
                .definition = method.definition,
                .module = module,
            },
        };
    }
};

pub const GenericInstantiation = struct {
    base_name: []const u8,
    args: []const Value,

    pub fn clone(self: GenericInstantiation, alloc: std.mem.Allocator) !GenericInstantiation {
        return .{
            .base_name = try alloc.dupe(u8, self.base_name),
            .args = try utils.cloneSlice(Value, self.args, alloc),
        };
    }
};

pub const Method = struct {
    name: []const u8,
    inner_name: []const u8,
    params: []const Function.Param,
    generic_params: []const Function.Param,
    return_type: *const Type,
    definition: ?*const ast.Statement.FunctionDefinition = null,

    pub fn clone(self: Method, alloc: std.mem.Allocator) !Method {
        return .{
            .name = self.name,
            .inner_name = try alloc.dupe(u8, self.inner_name),
            .params = try utils.cloneSlice(Function.Param, self.params, alloc),
            .generic_params = try utils.cloneSlice(Function.Param, self.generic_params, alloc),
            .return_type = try self.return_type.clonePtr(alloc),
            .definition = if (self.definition) |def| b: {
                const d = try alloc.create(ast.Statement.FunctionDefinition);
                d.* = try def.clone(alloc);
                break :b d;
            } else null,
        };
    }
};

pub fn CompoundType(T: utils.CompoundTypeTag) type {
    return struct {
        pub const MemberType = switch (T) {
            .@"struct", .@"union" => Type,
            .@"enum" => usize,
        };

        pub const Definition = switch (T) {
            .@"struct" => ast.Statement.StructDeclaration,
            .@"union" => ast.Statement.UnionDeclaration,
            .@"enum" => ast.Statement.EnumDeclaration,
        };

        name: []const u8,
        inner_name: []const u8,
        variables: *std.StringHashMap(Variable),
        subtypes: *std.StringHashMap(Subtype),
        members: *std.StringHashMap(MemberType),
        methods: *std.StringHashMap(Method),
        generic_params: []Function.Param,
        tag_type: ?*const Type, // only for unions and enums
        definition: ?*const Definition,
        module: Module,
        generic_instantiation: ?GenericInstantiation = null,

        /// get member or method. returns `null` if no member or method is found with `name`.
        pub fn getProperty(self: *const CompoundType(T), name: []const u8) ?union(enum) {
            variable: Variable,
            member: MemberType,
            method: Method,
            subtype: Subtype,
        } {
            const member = self.members.get(name);
            const method = self.methods.get(name);
            const variable = self.variables.get(name);
            const subtype = self.subtypes.get(name);

            var n: usize = 0;
            if (member != null) n += 1;
            if (method != null) n += 1;
            if (variable != null) n += 1;
            if (subtype != null) n += 1;
            if (n > 1) unreachable;

            return if (member) |m|
                .{ .member = m }
            else if (method) |m|
                .{ .method = m }
            else if (variable) |v|
                .{ .variable = v }
            else if (subtype) |s|
                .{ .subtype = s }
            else
                null;
        }

        pub fn getMember(self: *const CompoundType(T), tag_name: []const u8) !struct {
            member_name: []const u8,
            member_type: MemberType,
        } {
            var members_it = self.members.iterator();
            var i: usize = 0;
            while (members_it.next()) |m| : (i += 1) {
                if (std.mem.eql(u8, tag_name, m.key_ptr.*))
                    return .{
                        .member_name = m.key_ptr.*,
                        .member_type = m.value_ptr.*,
                    };
            }

            return error.NoSuchMember;
        }

        pub fn init(compiler: *Compiler, name: []const u8, inner_name: []const u8, tag_type: ?Type) !CompoundType(T) {
            if (T == .@"struct" and tag_type != null) @panic("Struct type can't have a tag type");

            const variables = try compiler.alloc.create(std.StringHashMap(Variable));
            variables.* = .init(compiler.alloc);

            const subtypes = try compiler.alloc.create(std.StringHashMap(Subtype));
            subtypes.* = .init(compiler.alloc);

            const members = try compiler.alloc.create(std.StringHashMap(MemberType));
            members.* = .init(compiler.alloc);

            const methods = try compiler.alloc.create(std.StringHashMap(Method));
            methods.* = .init(compiler.alloc);

            var tag: ?*Type = null;
            if (tag_type) |t| {
                tag = try compiler.alloc.create(Type);
                tag.?.* = t;
            }

            return .{
                .name = name,
                .inner_name = inner_name,
                .variables = variables,
                .subtypes = subtypes,
                .members = members,
                .methods = methods,
                .generic_params = &.{},
                .tag_type = tag,
                .definition = null,
                .module = compiler.module,
                .generic_instantiation = null,
            };
        }
    };
}

/// Returns a type from an AST compound type declaration statement.
pub fn fromCompoundTypeDeclaration(
    compiler: *Compiler,
    comptime T: utils.CompoundTypeTag,
    type_decl: *const switch (T) {
        .@"struct" => ast.Statement.StructDeclaration,
        .@"union" => ast.Statement.UnionDeclaration,
        .@"enum" => ast.Statement.EnumDeclaration,
    },
    opts: struct {
        inner_name: ?[]const u8 = null,
    },
) CompilerError!switch (T) {
    .@"struct" => Type.Struct,
    .@"union" => Type.Union,
    .@"enum" => Type.Enum,
} {
    const inner_name = opts.inner_name orelse try compiler.mangle(type_decl.name);
    const tag_type_inner_name = try std.fmt.allocPrint(compiler.alloc, "{s}_tag_type", .{type_decl.name});

    const should_define = if (compiler.getSymbolDefined(type_decl.name)) |sd| b: {
        if (sd)
            return errors.symbolShadowing(type_decl.name, type_decl.pos)
        else
            break :b false;
    } else |err| switch (err) {
        error.SymbolNotVariable => return err,
        else => true,
    };

    // register struct in scope
    var compound_type: switch (T) {
        .@"struct" => Type.Struct,
        .@"union" => Type.Union,
        .@"enum" => Type.Enum,
    } = if (compiler.getSymbolType(type_decl.name)) |existing| b: {
        switch (T) {
            .@"struct" => if (existing == .@"struct") break :b existing.@"struct",
            .@"union" => if (existing == .@"union") break :b existing.@"union",
            .@"enum" => if (existing == .@"enum") break :b existing.@"enum",
        }
        // If type mismatch or not found (logic error in scan?), create new.
        // But strict forward decl implies we should find it.
        // However, we fallback to init for safety/standalone usage.
        break :b try compoundTypeInit(compiler, T, type_decl, inner_name, tag_type_inner_name);
    } else |_| try compoundTypeInit(compiler, T, type_decl, inner_name, tag_type_inner_name);

    // If definition is set, it means we already populated this type.
    if (compound_type.definition != null) return compound_type;

    if (T == .@"union" and compound_type.tag_type == null) {
        const enum_decl = try compiler.alloc.create(ast.Statement.EnumDeclaration);
        enum_decl.* = .{
            .pos = type_decl.pos,
            .is_pub = false,
            .name = tag_type_inner_name,
            .variables = &.{},
            .subtypes = &.{},
            .members = b: {
                const members = try compiler.alloc.alloc(ast.Statement.EnumDeclaration.Member, type_decl.members.len);
                for (type_decl.members, 0..) |member, i| members[i] = .{ .name = member.name };
                break :b members;
            },
            .methods = &.{},
        };

        const tag_type_val: Type = .{ .@"enum" = try Type.fromCompoundTypeDeclaration(compiler, .@"enum", enum_decl, .{}) };

        const tag_ptr = try compiler.alloc.create(Type);
        tag_ptr.* = tag_type_val;
        compound_type.tag_type = tag_ptr;
    }

    switch (T) {
        .@"struct", .@"union" => for (type_decl.generic_types) |g| {
            // const generics: std.ArrayList(Type.types.Function.Param) = try .fromOwnedSlice(compound_type.generic_params);
            compound_type.generic_params = try compiler.alloc.realloc(compound_type.generic_params, compound_type.generic_params.len + 1);
            compound_type.generic_params[compound_type.generic_params.len - 1] = .{
                .name = g.name,
                .type = if (g.type == .inferred) .type_type else try .fromAst(compiler, g.type),
            };
        },
        else => {},
    }

    compound_type.definition = type_decl;

    try compiler.registerSymbol(
        type_decl.name,
        .{ .type = @unionInit(Type, @tagName(T), compound_type) },
        .{ .is_defined = should_define, .inner_name = try compiler.mangle(type_decl.name) },
    );

    if (T == .@"struct" or T == .@"union") {
        if (type_decl.generic_types.len > 0) return compound_type;
    }

    switch (T) {
        .@"struct", .@"union" => for (type_decl.generic_types) |g|
            try compiler.registerSymbol(g.name, .{ .type = .{ .generic_param = g.name } }, .{}),
        else => {},
    }

    for (type_decl.variables) |variable| {
        const value = try compiler.solveComptimeExpression(variable.assigned_value);
        const var_type: Type = if (variable.type == .inferred) value.getType() else try .fromAst(compiler, variable.type);

        try compiler.registerSymbol(
            variable.variable_name,
            if (variable.binding == .@"const")
                .{ .constant = .{ .type = var_type, .value = value } }
            else
                .{ .symbol = .{ .is_mut = variable.binding == .let_mut, .type = var_type } },
            .{},
        );

        try compound_type.variables.put(
            variable.variable_name,
            .{
                .is_pub = variable.is_pub,
                .inner_name = try std.fmt.allocPrint(
                    compiler.alloc,
                    "{s}_{s}",
                    .{ inner_name, variable.variable_name },
                ),
                .type = var_type,
                .value = value,
                .binding = variable.binding,
            },
        );
    }

    for (type_decl.subtypes) |subtype| switch (subtype) {
        inline else => |st, tag| {
            const st_ptr = try compiler.alloc.create(@TypeOf(st));
            st_ptr.* = st;
            try compound_type.subtypes.put(st.name, .{
                .is_pub = st.is_pub,
                .inner_name = try std.fmt.allocPrint(
                    compiler.alloc,
                    "{s}_{s}",
                    .{ inner_name, st.name },
                ),
                .type = @unionInit(
                    Type.Subtype.Tag,
                    @tagName(tag),
                    try fromCompoundTypeDeclaration(compiler, tag, st_ptr, .{
                        .inner_name = try std.fmt.allocPrint(
                            compiler.alloc,
                            "{s}_{s}",
                            .{ inner_name, st.name },
                        ),
                    }),
                ),
            });
        },
    };

    var enum_last_value: usize = 0;
    for (type_decl.members) |member| {
        if (compound_type.getProperty(member.name)) |_| return utils.printErr(
            error.DuplicateMember,
            "comperr: Duplicate member '{s}' declared in '{s}' at {f}.\n",
            .{ member.name, type_decl.name, type_decl.pos },
            .red,
        );

        switch (T) {
            inline .@"struct", .@"union" => {
                const member_type: Type = switch (T) {
                    .@"struct" => try .fromAst(compiler, member.type),
                    .@"union" => if (member.type) |t| try .fromAst(compiler, t) else .void,
                    else => unreachable,
                };
                switch (member_type) {
                    inline .@"struct", .@"union" => |ct, tag| {
                        var inner_members = ct.members.iterator();
                        while (inner_members.next()) |inner_member| {
                            if (inner_member.value_ptr.eql(@unionInit(Type, @tagName(T), compound_type)))
                                return utils.printErr(
                                    error.CircularTypeDefinition,
                                    "comperr: {s} '{s}' depends on itself. Member '{s}' is of type '{s}' ({f}).\n",
                                    .{ @tagName(tag), compound_type.name, inner_member.key_ptr.*, compound_type.name, type_decl.pos },
                                    .red,
                                );
                        }
                    },
                    else => {},
                }

                try compound_type.members.put(member.name, member_type);
            },
            .@"enum" => try compound_type.members.put(member.name, if (member.value) |value|
                (try compiler.solveComptimeExpression(value)).u64
            else b: {
                const val = enum_last_value;
                enum_last_value += 1;
                break :b val;
            }),
        }
    }

    for (type_decl.methods, 0..) |method, i| {
        const params = try compiler.alloc.alloc(Function.Param, method.parameters.len);
        for (method.parameters, 0..) |p, j| params[j] = .{
            .name = p.name,
            .type = try .fromAst(compiler, p.type),
        };

        const generic_params = try compiler.alloc.alloc(Function.Param, method.generic_parameters.len);
        for (method.generic_parameters, 0..) |p, j| generic_params[j] = .{
            .name = p.name,
            .type = if (p.type == .inferred) .type_type else try .fromAst(compiler, p.type),
        };

        const return_type: *const Type = try .fromAstPtr(compiler, method.return_type);
        try compound_type.methods.put(method.name, .{
            .name = method.name,
            .inner_name = try std.fmt.allocPrint(compiler.alloc, "__zag_{s}_{s}", .{
                type_decl.name,
                method.name,
            }),
            .generic_params = generic_params,
            .params = params,
            .return_type = return_type,
            .definition = &type_decl.methods[i],
        });
    }

    return compound_type;
}

fn compoundTypeInit(
    compiler: *Compiler,
    comptime T: utils.CompoundTypeTag,
    type_decl: *const switch (T) {
        .@"struct" => ast.Statement.StructDeclaration,
        .@"union" => ast.Statement.UnionDeclaration,
        .@"enum" => ast.Statement.EnumDeclaration,
    },
    inner_name: []const u8,
    tag_type_inner_name: []const u8,
) !switch (T) {
    .@"struct" => Type.Struct,
    .@"union" => Type.Union,
    .@"enum" => Type.Enum,
} {
    return try Type.CompoundType(T).init(compiler, type_decl.name, inner_name, switch (T) {
        .@"struct" => null,
        .@"enum" => .getTagType(type_decl.members.len),
        .@"union" => b: {
            const enum_decl = try compiler.alloc.create(ast.Statement.EnumDeclaration);
            enum_decl.* = .{
                .pos = type_decl.pos,
                .is_pub = false,
                .name = tag_type_inner_name,
                .variables = &.{},
                .subtypes = &.{},
                .members = b2: {
                    const members = try compiler.alloc.alloc(ast.Statement.EnumDeclaration.Member, type_decl.members.len);
                    for (type_decl.members, 0..) |member, i| members[i] = .{ .name = member.name };
                    break :b2 members;
                },
                .methods = &.{},
            };

            // Create the tag type but DON'T compile/emit it yet
            break :b .{ .@"enum" = try Type.fromCompoundTypeDeclaration(compiler, .@"enum", enum_decl, .{}) };
        },
    });
}

pub const Variable = struct {
    is_pub: bool,
    type: Type,
    inner_name: []const u8,
    value: Value,
    binding: utils.Binding,

    pub fn clone(self: Variable, alloc: std.mem.Allocator) !Variable {
        return .{
            .is_pub = self.is_pub,
            .type = try self.type.clone(alloc),
            .inner_name = try alloc.dupe(u8, self.inner_name),
            .value = try self.value.clone(alloc),
            .binding = self.binding,
        };
    }
};

pub const Subtype = struct {
    pub const Tag = union(utils.CompoundTypeTag) {
        @"struct": Struct,
        @"enum": Enum,
        @"union": Union,
    };

    is_pub: bool,
    type: Tag,
    inner_name: []const u8,

    pub fn toType(self: Subtype) Type {
        return switch (self.type) {
            inline else => |s, t| @unionInit(Type, @tagName(t), s),
        };
    }

    pub fn clone(self: Subtype, alloc: std.mem.Allocator) !Subtype {
        return .{
            .is_pub = self.is_pub,
            .type = switch (self.type) {
                inline else => |ct, t| @unionInit(Tag, @tagName(t), @field(
                    try @unionInit(Type, @tagName(t), ct).clone(alloc),
                    @tagName(t),
                )),
            },
            .inner_name = try alloc.dupe(u8, self.inner_name),
        };
    }
};

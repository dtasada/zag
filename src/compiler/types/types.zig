const std = @import("std");
const ast = @import("ast");
const utils = @import("utils");
const Compiler = @import("../Compiler.zig");
const Module = @import("../Module.zig");
const Value = @import("../Value.zig").Value;
const Type = @import("Type.zig").Type;

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
        members: *std.StringArrayHashMap(MemberType),
        methods: *std.StringArrayHashMap(Method),
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

            const members = try compiler.alloc.create(std.StringArrayHashMap(MemberType));
            members.* = .init(compiler.alloc);

            const methods = try compiler.alloc.create(std.StringArrayHashMap(Method));
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

const std = @import("std");
const utils = @import("utils");

const ast = @import("ast.zig");
const Expression = ast.Expression;
const Type = ast.Type;

pub const Statement = union(enum) {
    @"break": struct { pos: usize },
    @"continue": struct { pos: usize },
    @"for": For,
    @"if": If,
    @"return": Return,
    @"while": While,
    binding_function_declaration: BindingFunctionDeclaration,
    binding_type_declaration: BindingTypeDeclaration,
    block: Expression.Block,
    expression: Expression,
    function_definition: FunctionDefinition,
    import: Import,
    enum_declaration: EnumDeclaration,
    struct_declaration: StructDeclaration,
    union_declaration: UnionDeclaration,
    variable_definition: VariableDefinition,
    block_eval: Expression,
    @"defer": struct { pos: usize, payload: *const Statement },

    pub const For = struct {
        pos: usize,
        iterator: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
    };

    pub const FunctionDefinition = struct {
        pos: usize,
        is_pub: bool,
        name: []const u8,
        generic_parameters: ast.ParameterList,
        parameters: ast.ParameterList,
        return_type: Type,
        body: ast.Block,

        pub fn getType(self: *const FunctionDefinition, alloc: std.mem.Allocator) !Type {
            return try Type.createFunctionType(alloc, self.pos, self.name, self.parameters, self.generic_parameters, &self.return_type);
        }

        pub fn clone(self: FunctionDefinition, alloc: std.mem.Allocator) !FunctionDefinition {
            return .{
                .pos = self.pos,
                .is_pub = self.is_pub,
                .name = try alloc.dupe(u8, self.name),
                .generic_parameters = try utils.cloneSlice(ast.VariableSignature, self.generic_parameters, alloc),
                .parameters = try utils.cloneSlice(ast.VariableSignature, self.parameters, alloc),
                .return_type = try self.return_type.clone(alloc),
                .body = try utils.cloneSlice(ast.Statement, self.body, alloc),
            };
        }

        pub fn deinit(self: FunctionDefinition, alloc: std.mem.Allocator) void {
            alloc.free(self.name);
            utils.deinitSlice(ast.VariableSignature, self.generic_parameters, alloc);
            utils.deinitSlice(ast.VariableSignature, self.parameters, alloc);
            self.return_type.deinit(alloc);
            utils.deinitSlice(Statement, self.body, alloc);
        }
    };

    pub const If = struct {
        pos: usize,
        condition: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
        @"else": ?*const Statement = null,
    };

    pub const Import = struct {
        pos: usize,
        module_name: []const []const u8,
        alias: ?[]const u8,

        pub fn deinit(self: Import, alloc: std.mem.Allocator) void {
            for (self.module_name) |name| alloc.free(name);
            alloc.free(self.module_name);
            if (self.alias) |a| alloc.free(a);
        }
    };

    pub const Return = struct {
        pos: usize,
        @"return": ?ast.Expression,
    };

    fn CompoundTypeDeclaration(comptime T: enum { @"struct", @"union" }) type {
        return struct {
            pub const Member = struct {
                name: []const u8,
                type: if (T == .@"struct") Type else ?Type,

                pub fn clone(self: Member, alloc: std.mem.Allocator) !Member {
                    return switch (T) {
                        .@"struct" => .{
                            .name = try alloc.dupe(u8, self.name),
                            .type = try self.type.clone(alloc),
                        },
                        .@"union" => .{
                            .name = try alloc.dupe(u8, self.name),
                            .type = if (self.type) |t| try t.clone(alloc) else null,
                        },
                    };
                }

                pub fn deinit(self: Member, alloc: std.mem.Allocator) void {
                    alloc.free(self.name);
                    switch (T) {
                        .@"struct" => self.type.deinit(alloc),
                        .@"union" => if (self.type) |t| t.deinit(alloc),
                    }
                }
            };
            pos: usize,
            is_pub: bool,
            name: []const u8,
            generic_types: ast.ParameterList,
            variables: []const VariableDefinition,
            subtypes: []const ast.Subtype,
            members: []const Member,
            methods: []const FunctionDefinition,

            pub fn clone(self: *const CompoundTypeDeclaration(T), alloc: std.mem.Allocator) std.mem.Allocator.Error!CompoundTypeDeclaration(T) {
                return .{
                    .pos = self.pos,
                    .is_pub = self.is_pub,
                    .name = try alloc.dupe(u8, self.name),
                    .generic_types = try utils.cloneSlice(ast.VariableSignature, self.generic_types, alloc),
                    .variables = try utils.cloneSlice(VariableDefinition, self.variables, alloc),
                    .subtypes = try utils.cloneSlice(ast.Subtype, self.subtypes, alloc),
                    .members = try utils.cloneSlice(Member, self.members, alloc),
                    .methods = try utils.cloneSlice(FunctionDefinition, self.methods, alloc),
                };
            }

            pub fn deinit(self: CompoundTypeDeclaration(T), alloc: std.mem.Allocator) void {
                alloc.free(self.name);
                utils.deinitSlice(ast.VariableSignature, self.generic_types, alloc);
                utils.deinitSlice(VariableDefinition, self.variables, alloc);
                utils.deinitSlice(ast.Subtype, self.subtypes, alloc);
                utils.deinitSlice(Member, self.members, alloc);
                utils.deinitSlice(FunctionDefinition, self.methods, alloc);
            }
        };
    }

    pub const StructDeclaration = CompoundTypeDeclaration(.@"struct");
    pub const UnionDeclaration = CompoundTypeDeclaration(.@"union");

    pub const EnumDeclaration = struct {
        pub const Member = struct {
            name: []const u8,
            value: ?ast.Expression = null,

            pub fn clone(self: Member, alloc: std.mem.Allocator) !Member {
                return .{
                    .name = try alloc.dupe(u8, self.name),
                    .value = if (self.value) |v| try v.clone(alloc) else null,
                };
            }

            pub fn deinit(self: Member, alloc: std.mem.Allocator) void {
                alloc.free(self.name);
                if (self.value) |v| v.deinit(alloc);
            }
        };

        pos: usize,
        is_pub: bool,
        name: []const u8,
        variables: []const VariableDefinition,
        subtypes: []const ast.Subtype,
        members: []const Member,
        methods: []const FunctionDefinition,

        pub fn clone(self: *const EnumDeclaration, alloc: std.mem.Allocator) !EnumDeclaration {
            return .{
                .pos = self.pos,
                .is_pub = self.is_pub,
                .name = try alloc.dupe(u8, self.name),
                .variables = try utils.cloneSlice(VariableDefinition, self.variables, alloc),
                .subtypes = try utils.cloneSlice(ast.Subtype, self.subtypes, alloc),
                .members = try utils.cloneSlice(Member, self.members, alloc),
                .methods = try utils.cloneSlice(FunctionDefinition, self.methods, alloc),
            };
        }

        pub fn deinit(self: EnumDeclaration, alloc: std.mem.Allocator) void {
            alloc.free(self.name);
            utils.deinitSlice(VariableDefinition, self.variables, alloc);
            utils.deinitSlice(ast.Subtype, self.subtypes, alloc);
            utils.deinitSlice(Member, self.members, alloc);
            utils.deinitSlice(FunctionDefinition, self.methods, alloc);
        }
    };

    pub const While = struct {
        pos: usize,
        condition: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
    };

    pub const BindingTypeDeclaration = struct {
        pos: usize,
        is_pub: bool,
        name: []const u8,
        type: utils.CompoundTypeTag,

        pub fn deinit(self: BindingTypeDeclaration, alloc: std.mem.Allocator) void {
            alloc.free(self.name);
        }
    };

    pub const BindingFunctionDeclaration = struct {
        pos: usize,
        is_pub: bool,
        name: []const u8,
        parameters: ast.ParameterList,
        return_type: Type,

        pub fn getType(self: *const BindingFunctionDeclaration, alloc: std.mem.Allocator) !Type {
            return try Type.createFunctionType(alloc, self.pos, self.name, self.parameters, &.{}, &self.return_type);
        }

        pub fn deinit(self: BindingFunctionDeclaration, alloc: std.mem.Allocator) void {
            alloc.free(self.name);
            utils.deinitSlice(ast.VariableSignature, self.parameters, alloc);
            self.return_type.deinit(alloc);
        }
    };

    pub const VariableDefinition = struct {
        pos: usize,
        is_pub: bool,
        binding: utils.Binding,
        variable_name: []const u8,
        type: Type,
        assigned_value: ast.Expression,

        pub fn clone(self: VariableDefinition, alloc: std.mem.Allocator) !VariableDefinition {
            return .{
                .pos = self.pos,
                .is_pub = self.is_pub,
                .binding = self.binding,
                .variable_name = try alloc.dupe(u8, self.variable_name),
                .type = try self.type.clone(alloc),
                .assigned_value = try self.assigned_value.clone(alloc),
            };
        }

        pub fn deinit(self: VariableDefinition, alloc: std.mem.Allocator) void {
            alloc.free(self.variable_name);
            self.type.deinit(alloc);
            self.assigned_value.deinit(alloc);
        }
    };

    fn clonePtr(self: Statement, alloc: std.mem.Allocator) !*Statement {
        const ret = try alloc.create(Statement);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Statement, alloc: std.mem.Allocator) std.mem.Allocator.Error!Statement {
        return switch (self) {
            .@"break" => |b| .{ .@"break" = .{ .pos = b.pos } },
            .@"continue" => |c| .{ .@"continue" = .{ .pos = c.pos } },
            .@"defer" => |@"defer"| .{
                .@"defer" = .{
                    .pos = @"defer".pos,
                    .payload = try @"defer".payload.clonePtr(alloc),
                },
            },
            .@"for" => |@"for"| .{
                .@"for" = .{
                    .pos = @"for".pos,
                    .iterator = try @"for".iterator.clone(alloc),
                    .capture = if (@"for".capture) |c| try c.clone(alloc) else null,
                    .body = try @"for".body.clonePtr(alloc),
                },
            },
            .@"if" => |@"if"| .{
                .@"if" = .{
                    .pos = @"if".pos,
                    .condition = try @"if".condition.clone(alloc),
                    .capture = if (@"if".capture) |c| try c.clone(alloc) else null,
                    .body = try @"if".body.clonePtr(alloc),
                    .@"else" = if (@"if".@"else") |e| try e.clonePtr(alloc) else null,
                },
            },
            .@"return" => |@"return"| .{
                .@"return" = .{
                    .pos = @"return".pos,
                    .@"return" = if (@"return".@"return") |r| try r.clone(alloc) else null,
                },
            },
            .@"while" => |@"while"| .{
                .@"while" = .{
                    .pos = @"while".pos,
                    .condition = try @"while".condition.clone(alloc),
                    .capture = if (@"while".capture) |c| try c.clone(alloc) else null,
                    .body = try @"while".body.clonePtr(alloc),
                },
            },
            .binding_function_declaration => |bfd| .{
                .binding_function_declaration = .{
                    .pos = bfd.pos,
                    .is_pub = bfd.is_pub,
                    .name = try alloc.dupe(u8, bfd.name),
                    .parameters = try utils.cloneSlice(ast.VariableSignature, bfd.parameters, alloc),
                    .return_type = try bfd.return_type.clone(alloc),
                },
            },
            .binding_type_declaration => |btd| .{
                .binding_type_declaration = .{
                    .pos = btd.pos,
                    .is_pub = btd.is_pub,
                    .name = try alloc.dupe(u8, btd.name),
                    .type = btd.type,
                },
            },
            .block => |block| .{
                .block = .{
                    .pos = block.pos,
                    .payload = try utils.cloneSlice(ast.Statement, block.payload, alloc),
                },
            },
            .import => |import| .{
                .import = .{
                    .pos = import.pos,
                    .module_name = b: {
                        const mn = try alloc.alloc([]const u8, import.module_name.len);
                        for (import.module_name, 0..) |name, i| mn[i] = try alloc.dupe(u8, name);
                        break :b mn;
                    },
                    .alias = if (import.alias) |a| try alloc.dupe(u8, a) else null,
                },
            },
            inline else => |other, t| @unionInit(Statement, @tagName(t), try other.clone(alloc)),
        };
    }

    fn deinitPtr(self: *const Statement, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Statement, alloc: std.mem.Allocator) void {
        switch (self) {
            inline .binding_type_declaration,
            .binding_function_declaration,
            .expression,
            .block_eval,
            .function_definition,
            .import,
            .struct_declaration,
            .union_declaration,
            .enum_declaration,
            .variable_definition,
            => |s| s.deinit(alloc),
            .@"for" => |s| {
                s.iterator.deinit(alloc);
                if (s.capture) |c| c.deinit(alloc);
                s.body.deinitPtr(alloc);
            },
            .@"if" => |s| {
                s.condition.deinit(alloc);
                if (s.capture) |c| c.deinit(alloc);
                s.body.deinitPtr(alloc);
                if (s.@"else") |e| e.deinitPtr(alloc);
            },
            .@"return" => |s| if (s.@"return") |r| r.deinit(alloc),
            .@"while" => |s| {
                s.condition.deinit(alloc);
                if (s.capture) |c| c.deinit(alloc);
                s.body.deinitPtr(alloc);
            },
            .block => |s| utils.deinitSlice(Statement, s.payload, alloc),
            .@"defer" => |s| s.payload.deinitPtr(alloc),
            else => {},
        }
    }
};

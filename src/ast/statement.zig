const std = @import("std");
const utils = @import("utils");

const ast = @import("ast.zig");
const Expression = ast.Expression;
const Type = ast.Type;

pub const Statement = union(enum) {
    @"break": struct { pos: utils.Position },
    @"continue": struct { pos: utils.Position },
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
    @"defer": struct { pos: utils.Position, payload: *const Statement },

    pub const For = struct {
        pos: utils.Position,
        iterator: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
    };

    pub const FunctionDefinition = struct {
        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        generic_parameters: ast.ParameterList,
        parameters: ast.ParameterList,
        return_type: Type,
        body: ast.Block,

        pub fn getType(self: *const FunctionDefinition) Type {
            return .{
                .function = .{
                    .name = self.name,
                    .pos = self.pos,
                    .generic_parameters = self.generic_parameters,
                    .parameters = self.parameters,
                    .return_type = &self.return_type,
                },
            };
        }

        pub fn clone(self: FunctionDefinition, alloc: std.mem.Allocator) !FunctionDefinition {
            return .{
                .pos = try self.pos.clone(alloc),
                .is_pub = self.is_pub,
                .name = try alloc.dupe(u8, self.name),
                .generic_parameters = try utils.cloneSlice(ast.VariableSignature, self.generic_parameters, alloc),
                .parameters = try utils.cloneSlice(ast.VariableSignature, self.parameters, alloc),
                .return_type = try self.return_type.clone(alloc),
                .body = try utils.cloneSlice(ast.Statement, self.body, alloc),
            };
        }

        pub fn deinit(self: FunctionDefinition, alloc: std.mem.Allocator) void {
            self.pos.deinit(alloc);
            alloc.free(self.name);
            utils.deinitSlice(ast.VariableSignature, self.generic_parameters, alloc);
            utils.deinitSlice(ast.VariableSignature, self.parameters, alloc);
            self.return_type.deinit(alloc);
            utils.deinitSlice(Statement, self.body, alloc);
        }
    };

    pub const If = struct {
        pos: utils.Position,
        condition: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
        @"else": ?*const Statement = null,
    };

    pub const Import = struct {
        pos: utils.Position,
        module_name: []const []const u8,
        alias: ?[]const u8,
    };

    pub const Return = struct {
        pos: utils.Position,
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
            pos: utils.Position,
            is_pub: bool,
            name: []const u8,
            generic_types: ast.ParameterList,
            variables: []const VariableDefinition,
            subtypes: []const ast.Subtype,
            members: []const Member,
            methods: []const FunctionDefinition,

            pub fn clone(self: *const CompoundTypeDeclaration(T), alloc: std.mem.Allocator) std.mem.Allocator.Error!CompoundTypeDeclaration(T) {
                return .{
                    .pos = try self.pos.clone(alloc),
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
                self.pos.deinit(alloc);
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

        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        variables: []const VariableDefinition,
        subtypes: []const ast.Subtype,
        members: []const Member,
        methods: []const FunctionDefinition,

        pub fn clone(self: *const EnumDeclaration, alloc: std.mem.Allocator) !EnumDeclaration {
            return .{
                .pos = try self.pos.clone(alloc),
                .is_pub = self.is_pub,
                .name = try alloc.dupe(u8, self.name),
                .variables = try utils.cloneSlice(VariableDefinition, self.variables, alloc),
                .subtypes = try utils.cloneSlice(ast.Subtype, self.subtypes, alloc),
                .members = try utils.cloneSlice(Member, self.members, alloc),
                .methods = try utils.cloneSlice(FunctionDefinition, self.methods, alloc),
            };
        }

        pub fn deinit(self: EnumDeclaration, alloc: std.mem.Allocator) void {
            self.pos.deinit(alloc);
            alloc.free(self.name);
            utils.deinitSlice(VariableDefinition, self.variables, alloc);
            utils.deinitSlice(ast.Subtype, self.subtypes, alloc);
            utils.deinitSlice(Member, self.members, alloc);
            utils.deinitSlice(FunctionDefinition, self.methods, alloc);
        }
    };

    pub const While = struct {
        pos: utils.Position,
        condition: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
    };

    pub const BindingTypeDeclaration = struct {
        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        type: utils.CompoundTypeTag,
    };

    pub const BindingFunctionDeclaration = struct {
        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        parameters: ast.ParameterList,
        return_type: Type,

        pub fn getType(self: *const BindingFunctionDeclaration) Type {
            return .{
                .function = .{
                    .pos = self.pos,
                    .name = self.name,
                    .generic_parameters = &.{},
                    .parameters = self.parameters,
                    .return_type = &self.return_type,
                },
            };
        }
    };

    pub const VariableDefinition = struct {
        pos: utils.Position,
        is_pub: bool,
        binding: utils.Binding,
        variable_name: []const u8,
        type: Type,
        assigned_value: ast.Expression,

        pub fn clone(self: VariableDefinition, alloc: std.mem.Allocator) !VariableDefinition {
            return .{
                .pos = try self.pos.clone(alloc),
                .is_pub = self.is_pub,
                .binding = self.binding,
                .variable_name = try alloc.dupe(u8, self.variable_name),
                .type = try self.type.clone(alloc),
                .assigned_value = try self.assigned_value.clone(alloc),
            };
        }

        pub fn deinit(self: VariableDefinition, alloc: std.mem.Allocator) void {
            self.pos.deinit(alloc);
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
            .@"break" => |b| .{ .@"break" = .{ .pos = try b.pos.clone(alloc) } },
            .@"continue" => |c| .{ .@"continue" = .{ .pos = try c.pos.clone(alloc) } },
            .@"defer" => |@"defer"| .{
                .@"defer" = .{
                    .pos = try @"defer".pos.clone(alloc),
                    .payload = try @"defer".payload.clonePtr(alloc),
                },
            },
            .@"for" => |@"for"| .{
                .@"for" = .{
                    .pos = try @"for".pos.clone(alloc),
                    .iterator = try @"for".iterator.clone(alloc),
                    .capture = if (@"for".capture) |c| try c.clone(alloc) else null,
                    .body = try @"for".body.clonePtr(alloc),
                },
            },
            .@"if" => |@"if"| .{
                .@"if" = .{
                    .pos = try @"if".pos.clone(alloc),
                    .condition = try @"if".condition.clone(alloc),
                    .capture = if (@"if".capture) |c| try c.clone(alloc) else null,
                    .body = try @"if".body.clonePtr(alloc),
                    .@"else" = if (@"if".@"else") |e| try e.clonePtr(alloc) else null,
                },
            },
            .@"return" => |@"return"| .{
                .@"return" = .{
                    .pos = try @"return".pos.clone(alloc),
                    .@"return" = if (@"return".@"return") |r| try r.clone(alloc) else null,
                },
            },
            .@"while" => |@"while"| .{
                .@"while" = .{
                    .pos = try @"while".pos.clone(alloc),
                    .condition = try @"while".condition.clone(alloc),
                    .capture = if (@"while".capture) |c| try c.clone(alloc) else null,
                    .body = try @"while".body.clonePtr(alloc),
                },
            },
            .binding_function_declaration => |bfd| .{
                .binding_function_declaration = .{
                    .pos = try bfd.pos.clone(alloc),
                    .is_pub = bfd.is_pub,
                    .name = try alloc.dupe(u8, bfd.name),
                    .parameters = try utils.cloneSlice(ast.VariableSignature, bfd.parameters, alloc),
                    .return_type = try bfd.return_type.clone(alloc),
                },
            },
            .binding_type_declaration => |btd| .{
                .binding_type_declaration = .{
                    .pos = try btd.pos.clone(alloc),
                    .is_pub = btd.is_pub,
                    .name = try alloc.dupe(u8, btd.name),
                    .type = btd.type,
                },
            },
            .block => |block| .{
                .block = .{
                    .pos = try block.pos.clone(alloc),
                    .payload = try utils.cloneSlice(ast.Statement, block.payload, alloc),
                },
            },
            .import => |import| .{
                .import = .{
                    .pos = try import.pos.clone(alloc),
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
            .@"break" => |s| s.pos.deinit(alloc),
            .@"continue" => |s| s.pos.deinit(alloc),
            .@"for" => |s| {
                s.pos.deinit(alloc);
                s.iterator.deinit(alloc);
                if (s.capture) |c| c.deinit(alloc);
                s.body.deinitPtr(alloc);
            },
            .@"if" => |s| {
                s.pos.deinit(alloc);
                s.condition.deinit(alloc);
                if (s.capture) |c| c.deinit(alloc);
                s.body.deinitPtr(alloc);
                if (s.@"else") |e| e.deinitPtr(alloc);
            },
            .@"return" => |s| {
                s.pos.deinit(alloc);
                if (s.@"return") |r| r.deinit(alloc);
            },
            .@"while" => |s| {
                s.pos.deinit(alloc);
                s.condition.deinit(alloc);
                if (s.capture) |c| c.deinit(alloc);
                s.body.deinitPtr(alloc);
            },
            .binding_function_declaration => |s| {
                s.pos.deinit(alloc);
                alloc.free(s.name);
                utils.deinitSlice(ast.VariableSignature, s.parameters, alloc);
                s.return_type.deinit(alloc);
            },
            .binding_type_declaration => |s| {
                s.pos.deinit(alloc);
                alloc.free(s.name);
            },
            .block => |s| {
                s.pos.deinit(alloc);
                utils.deinitSlice(Statement, s.payload, alloc);
            },
            .expression, .block_eval => |s| s.deinit(alloc),
            .function_definition => |s| s.deinit(alloc),
            .import => |s| {
                s.pos.deinit(alloc);
                for (s.module_name) |name| alloc.free(name);
                alloc.free(s.module_name);
                if (s.alias) |a| alloc.free(a);
            },
            .struct_declaration => |s| s.deinit(alloc),
            .union_declaration => |s| s.deinit(alloc),
            .enum_declaration => |s| s.deinit(alloc),
            .@"defer" => |s| {
                s.pos.deinit(alloc);
                s.payload.deinitPtr(alloc);
            },
            .variable_definition => |s| s.deinit(alloc),
        }
    }
};

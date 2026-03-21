const std = @import("std");
const utils = @import("utils");

const Token = @import("lexer").Token;

const ast = @import("ast.zig");
const Statement = @import("statement.zig").Statement;
const Expression = @import("expression.zig").Expression;
const Type = @import("type.zig").Type;

pub const TopLevelStatement = union(enum) {
    binding_function_declaration: BindingFunctionDeclaration,
    binding_type_declaration: BindingTypeDeclaration,
    function_definition: FunctionDefinition,
    variable_definition: Statement.VariableDefinition,
    import: Import,
    enum_declaration: EnumDeclaration,
    struct_declaration: StructDeclaration,
    union_declaration: UnionDeclaration,

    pub fn clone(self: TopLevelStatement, alloc: std.mem.Allocator) !TopLevelStatement {
        return switch (self) {
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
        };
    }

    pub fn deinit(self: TopLevelStatement, alloc: std.mem.Allocator) void {
        switch (self) {
            inline else => |s| s.deinit(alloc),
        }
    }

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
            variables: []const Statement.VariableDefinition,
            subtypes: []const Subtype,
            members: []const Member,
            methods: []const FunctionDefinition,

            pub fn clone(self: *const CompoundTypeDeclaration(T), alloc: std.mem.Allocator) std.mem.Allocator.Error!CompoundTypeDeclaration(T) {
                return .{
                    .pos = self.pos,
                    .is_pub = self.is_pub,
                    .name = try alloc.dupe(u8, self.name),
                    .generic_types = try utils.cloneSlice(ast.VariableSignature, self.generic_types, alloc),
                    .variables = try utils.cloneSlice(Statement.VariableDefinition, self.variables, alloc),
                    .subtypes = try utils.cloneSlice(Subtype, self.subtypes, alloc),
                    .members = try utils.cloneSlice(Member, self.members, alloc),
                    .methods = try utils.cloneSlice(Statement.FunctionDefinition, self.methods, alloc),
                };
            }

            pub fn deinit(self: CompoundTypeDeclaration(T), alloc: std.mem.Allocator) void {
                alloc.free(self.name);
                utils.deinitSlice(ast.VariableSignature, self.generic_types, alloc);
                utils.deinitSlice(Statement.VariableDefinition, self.variables, alloc);
                utils.deinitSlice(Subtype, self.subtypes, alloc);
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
            value: ?Expression = null,

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
        variables: []const Statement.VariableDefinition,
        subtypes: []const Subtype,
        members: []const Member,
        methods: []const FunctionDefinition,

        pub fn clone(self: *const EnumDeclaration, alloc: std.mem.Allocator) !EnumDeclaration {
            return .{
                .pos = self.pos,
                .is_pub = self.is_pub,
                .name = try alloc.dupe(u8, self.name),
                .variables = try utils.cloneSlice(Statement.VariableDefinition, self.variables, alloc),
                .subtypes = try utils.cloneSlice(Subtype, self.subtypes, alloc),
                .members = try utils.cloneSlice(Member, self.members, alloc),
                .methods = try utils.cloneSlice(Statement.FunctionDefinition, self.methods, alloc),
            };
        }

        pub fn deinit(self: EnumDeclaration, alloc: std.mem.Allocator) void {
            alloc.free(self.name);
            utils.deinitSlice(Statement.VariableDefinition, self.variables, alloc);
            utils.deinitSlice(Subtype, self.subtypes, alloc);
            utils.deinitSlice(Member, self.members, alloc);
            utils.deinitSlice(FunctionDefinition, self.methods, alloc);
        }
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
                .body = try utils.cloneSlice(Statement, self.body, alloc),
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

    pub const Subtype = union(utils.CompoundTypeTag) {
        @"struct": TopLevelStatement.StructDeclaration,
        @"enum": TopLevelStatement.EnumDeclaration,
        @"union": TopLevelStatement.UnionDeclaration,

        pub fn clone(self: Subtype, alloc: std.mem.Allocator) std.mem.Allocator.Error!Subtype {
            return switch (self) {
                .@"struct" => |*s| .{ .@"struct" = try s.clone(alloc) },
                .@"enum" => |*e| .{ .@"enum" = try e.clone(alloc) },
                .@"union" => |*u| .{ .@"union" = try u.clone(alloc) },
            };
        }

        pub fn deinit(self: Subtype, alloc: std.mem.Allocator) void {
            switch (self) {
                inline else => |s| s.deinit(alloc),
            }
        }
    };
};

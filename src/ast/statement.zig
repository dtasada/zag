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
    @"while": While,
    @"return": Return,
    block: Expression.Block,
    expression: Expression,
    block_eval: Expression,
    variable_definition: Statement.VariableDefinition,
    @"defer": struct { pos: usize, payload: *const Statement },

    pub const For = struct {
        pos: usize,
        iterator: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
    };

    pub const If = struct {
        pos: usize,
        condition: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
        @"else": ?*const Statement = null,
    };

    pub const Return = struct {
        pos: usize,
        @"return": ?ast.Expression,
    };

    pub const While = struct {
        pos: usize,
        condition: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
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
            .block => |block| .{
                .block = .{
                    .pos = block.pos,
                    .payload = try utils.cloneSlice(ast.Statement, block.payload, alloc),
                },
            },
            inline else => |other, t| @unionInit(Statement, @tagName(t), try other.clone(alloc)),
        };
    }

    pub fn deinitPtr(self: *const Statement, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Statement, alloc: std.mem.Allocator) void {
        switch (self) {
            inline .expression, .block_eval, .variable_definition => |s| s.deinit(alloc),
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
            .@"continue", .@"break" => {},
        }
    }
};

const std = @import("std");
const utils = @import("utils");

const ast = @import("ast.zig");
const Statement = ast.Statement;
const Type = ast.Type;

pub const Expression = union(enum) {
    bad_node: struct { pos: usize },

    // literals
    ident: struct { pos: usize, payload: []const u8 },
    string: struct { pos: usize, payload: []const u8 },
    char: struct { pos: usize, payload: u8 },
    int: struct { pos: usize, payload: usize },
    float: struct { pos: usize, payload: f64 },

    @"if": If,
    array_instantiation: ArrayInstantiation,
    assignment: Assignment,
    binary: Binary,
    block: Expression.Block,
    call: Call,
    comparison: Comparison,
    dereference: Dereference,
    generic: Generic,
    index: Index,
    slice: Slice,
    match: Match,
    member: Member,
    prefix: Prefix,
    range: Range,
    reference: Reference,
    struct_instantiation: StructInstantiation,
    type: struct { pos: usize, payload: Type },
    @"try": struct { pos: usize, payload: *const Expression },
    @"catch": struct {
        pos: usize,
        lhs: *const Expression,
        rhs: *const Expression,
    },

    pub const Match = struct {
        pub const Case = struct {
            pub const Condition = union(enum) {
                opts: []const Expression,
                @"else",
            };

            pos: usize,
            condition: Condition,
            result: Statement,

            pub fn clone(self: Case, alloc: std.mem.Allocator) !Case {
                return .{
                    .pos = self.pos,
                    .condition = switch (self.condition) {
                        .opts => |opts| .{ .opts = try utils.cloneSlice(ast.Expression, opts, alloc) },
                        .@"else" => .@"else",
                    },
                    .result = try self.result.clone(alloc),
                };
            }

            pub fn deinit(self: Case, alloc: std.mem.Allocator) void {
                if (self.condition == .opts) utils.deinitSlice(Expression, self.condition.opts, alloc);
                self.result.deinit(alloc);
            }
        };

        pos: usize,
        condition: *const Expression,
        cases: []const Case,
    };

    pub const Block = struct { pos: usize, payload: ast.Block };
    pub const Generic = struct {
        pos: usize,
        lhs: *const Expression,
        arguments: ast.ArgumentList,
    };

    pub const Binary = struct {
        pos: usize,
        lhs: *const Expression,
        op: ast.BinaryOperator,
        rhs: *const Expression,
    };

    pub const Comparison = struct {
        pub const Item = struct {
            op: ast.BinaryOperator,
            right: *const Expression,

            pub fn clone(self: Item, alloc: std.mem.Allocator) !Item {
                return .{ .op = self.op, .right = try self.right.clonePtr(alloc) };
            }

            pub fn deinit(self: Item, alloc: std.mem.Allocator) void {
                self.right.deinitPtr(alloc);
            }
        };

        pos: usize,
        left: *const Expression,
        comparisons: []const Item,
    };

    pub const Member = struct {
        pos: usize,
        parent: *const Expression,
        member_name: []const u8,
    };

    pub const Dereference = struct {
        pos: usize,
        parent: *const Expression,
    };

    pub const Call = struct {
        pos: usize,
        callee: *const Expression,
        args: ast.ArgumentList,
    };

    pub const Prefix = struct {
        pos: usize,
        op: ast.PrefixOperator,
        rhs: *const Expression,
    };

    pub const Assignment = struct {
        pos: usize,
        assignee: *const Expression,
        op: ast.AssignmentOperator,
        value: *const Expression,
    };

    pub const StructInstantiation = struct {
        pub const Member = struct {
            name: []const u8,
            value: Expression,

            pub fn clone(
                self: StructInstantiation.Member,
                alloc: std.mem.Allocator,
            ) !StructInstantiation.Member {
                return .{
                    .name = try alloc.dupe(u8, self.name),
                    .value = try self.value.clone(alloc),
                };
            }

            pub fn deinit(self: StructInstantiation.Member, alloc: std.mem.Allocator) void {
                alloc.free(self.name);
                self.value.deinit(alloc);
            }
        };

        pos: usize,
        type_expr: *const Expression,
        members: []const StructInstantiation.Member,
    };

    pub const ArrayInstantiation = struct {
        pos: usize,
        length: *const Expression,
        type: Type,
        contents: []const Expression,
    };

    const Range = struct {
        pos: usize,
        start: *const Expression,
        end: ?*const Expression,
        inclusive: bool,
    };

    const Reference = struct {
        pos: usize,
        inner: *const Expression,
        is_mut: bool,
    };

    pub const If = struct {
        pos: usize,
        condition: *const Expression,
        capture: ?utils.Capture,
        body: *const Expression,
        @"else": *const Expression,
    };

    pub const Index = struct {
        pos: usize,
        lhs: *const Expression,
        index: *const Expression,
    };

    pub const Slice = struct {
        pos: usize,
        lhs: *const Expression,
        start: *const Expression,
        end: ?*const Expression,
        inclusive: bool,
    };

    pub inline fn pos(self: *const Expression) usize {
        return switch (self.*) {
            inline else => |some| some.pos,
        };
    }

    pub fn clonePtr(self: Expression, alloc: std.mem.Allocator) !*Expression {
        const ret = try alloc.create(Expression);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Expression, alloc: std.mem.Allocator) std.mem.Allocator.Error!Expression {
        return switch (self) {
            inline .ident, .string => |s, t| @unionInit(Expression, @tagName(t), .{
                .pos = s.pos,
                .payload = try alloc.dupe(u8, s.payload),
            }),
            inline .int, .float, .char => |n, t| @unionInit(Expression, @tagName(t), .{
                .pos = n.pos,
                .payload = n.payload,
            }),
            .bad_node => |bn| .{ .bad_node = .{ .pos = bn.pos } },
            .block => |block| .{
                .block = .{
                    .pos = block.pos,
                    .payload = try utils.cloneSlice(Statement, block.payload, alloc),
                },
            },
            .generic => |generic| .{
                .generic = .{
                    .pos = generic.pos,
                    .lhs = try generic.lhs.clonePtr(alloc),
                    .arguments = try utils.cloneSlice(ast.Expression, generic.arguments, alloc),
                },
            },
            .binary => |binary| .{
                .binary = .{
                    .pos = binary.pos,
                    .lhs = try binary.lhs.clonePtr(alloc),
                    .op = binary.op,
                    .rhs = try binary.rhs.clonePtr(alloc),
                },
            },
            .comparison => |comparison| .{
                .comparison = .{
                    .pos = comparison.pos,
                    .left = try comparison.left.clonePtr(alloc),
                    .comparisons = try utils.cloneSlice(Comparison.Item, comparison.comparisons, alloc),
                },
            },
            .member => |member| .{
                .member = .{
                    .pos = member.pos,
                    .parent = try member.parent.clonePtr(alloc),
                    .member_name = try alloc.dupe(u8, member.member_name),
                },
            },
            .dereference => |dereference| .{
                .dereference = .{
                    .pos = dereference.pos,
                    .parent = try dereference.parent.clonePtr(alloc),
                },
            },
            .call => |call| .{
                .call = .{
                    .pos = call.pos,
                    .callee = try call.callee.clonePtr(alloc),
                    .args = try utils.cloneSlice(ast.Expression, call.args, alloc),
                },
            },
            .prefix => |prefix| .{
                .prefix = .{
                    .pos = prefix.pos,
                    .op = prefix.op,
                    .rhs = try prefix.rhs.clonePtr(alloc),
                },
            },
            .assignment => |assignment| .{
                .assignment = .{
                    .pos = assignment.pos,
                    .assignee = try assignment.assignee.clonePtr(alloc),
                    .op = assignment.op,
                    .value = try assignment.value.clonePtr(alloc),
                },
            },
            .struct_instantiation => |si| .{
                .struct_instantiation = .{
                    .pos = si.pos,
                    .type_expr = try si.type_expr.clonePtr(alloc),
                    .members = try utils.cloneSlice(StructInstantiation.Member, si.members, alloc),
                },
            },
            .array_instantiation => |ai| .{
                .array_instantiation = .{
                    .pos = ai.pos,
                    .length = try ai.length.clonePtr(alloc),
                    .type = try ai.type.clone(alloc),
                    .contents = try utils.cloneSlice(ast.Expression, ai.contents, alloc),
                },
            },
            .range => |range| .{
                .range = .{
                    .pos = range.pos,
                    .start = try range.start.clonePtr(alloc),
                    .end = if (range.end) |e| try e.clonePtr(alloc) else null,
                    .inclusive = range.inclusive,
                },
            },
            .reference => |reference| .{
                .reference = .{
                    .pos = reference.pos,
                    .inner = try reference.inner.clonePtr(alloc),
                    .is_mut = reference.is_mut,
                },
            },
            .@"if" => |@"if"| .{
                .@"if" = .{
                    .pos = @"if".pos,
                    .condition = try @"if".condition.clonePtr(alloc),
                    .capture = if (@"if".capture) |c| try c.clone(alloc) else null,
                    .body = try @"if".body.clonePtr(alloc),
                    .@"else" = try @"if".@"else".clonePtr(alloc),
                },
            },
            .index => |index| .{
                .index = .{
                    .pos = index.pos,
                    .lhs = try index.lhs.clonePtr(alloc),
                    .index = try index.index.clonePtr(alloc),
                },
            },
            .slice => |slice| .{
                .slice = .{
                    .pos = slice.pos,
                    .lhs = try slice.lhs.clonePtr(alloc),
                    .start = try slice.start.clonePtr(alloc),
                    .end = if (slice.end) |e| try e.clonePtr(alloc) else null,
                    .inclusive = slice.inclusive,
                },
            },
            .match => |match| .{
                .match = .{
                    .pos = match.pos,
                    .condition = try match.condition.clonePtr(alloc),
                    .cases = try utils.cloneSlice(Expression.Match.Case, match.cases, alloc),
                },
            },
            .type => |t| .{ .type = .{ .pos = t.pos, .payload = try t.payload.clone(alloc) } },
            .@"try" => |@"try"| .{
                .@"try" = .{
                    .pos = @"try".pos,
                    .payload = try @"try".payload.clonePtr(alloc),
                },
            },
            .@"catch" => |@"catch"| .{
                .@"catch" = .{
                    .pos = @"catch".pos,
                    .lhs = try @"catch".lhs.clonePtr(alloc),
                    .rhs = try @"catch".rhs.clonePtr(alloc),
                },
            },
        };
    }

    pub fn deinitPtr(self: *const Expression, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Expression, alloc: std.mem.Allocator) void {
        switch (self) {
            else => {},
            .ident => |s| alloc.free(s.payload),
            .string => |s| alloc.free(s.payload),
            .@"if" => |s| {
                s.condition.deinitPtr(alloc);
                if (s.capture) |c| c.deinit(alloc);
                s.body.deinitPtr(alloc);
                s.@"else".deinitPtr(alloc);
            },
            .array_instantiation => |s| {
                s.length.deinitPtr(alloc);
                s.type.deinit(alloc);
                utils.deinitSlice(Expression, s.contents, alloc);
            },
            .assignment => |s| {
                s.assignee.deinitPtr(alloc);
                s.value.deinitPtr(alloc);
            },
            .binary => |s| {
                s.lhs.deinitPtr(alloc);
                s.rhs.deinitPtr(alloc);
            },
            .block => |s| utils.deinitSlice(Statement, s.payload, alloc),
            .call => |s| {
                s.callee.deinitPtr(alloc);
                utils.deinitSlice(Expression, s.args, alloc);
            },
            .comparison => |s| {
                s.left.deinitPtr(alloc);
                utils.deinitSlice(Comparison.Item, s.comparisons, alloc);
            },
            .dereference => |s| s.parent.deinitPtr(alloc),
            .generic => |s| {
                s.lhs.deinitPtr(alloc);
                utils.deinitSlice(Expression, s.arguments, alloc);
            },
            .index => |s| {
                s.lhs.deinitPtr(alloc);
                s.index.deinitPtr(alloc);
            },
            .slice => |s| {
                s.lhs.deinitPtr(alloc);
                s.start.deinitPtr(alloc);
                if (s.end) |end| end.deinitPtr(alloc);
            },
            .match => |s| {
                s.condition.deinitPtr(alloc);
                utils.deinitSlice(Match.Case, s.cases, alloc);
            },
            .member => |s| {
                s.parent.deinitPtr(alloc);
                alloc.free(s.member_name);
            },
            .prefix => |s| {
                s.rhs.deinitPtr(alloc);
            },
            .range => |s| {
                s.start.deinitPtr(alloc);
                if (s.end) |end| end.deinitPtr(alloc);
            },
            .reference => |s| s.inner.deinitPtr(alloc),
            .struct_instantiation => |s| {
                s.type_expr.deinitPtr(alloc);
                utils.deinitSlice(StructInstantiation.Member, s.members, alloc);
            },
            .type => |s| s.payload.deinit(alloc),
            .@"try" => |s| s.payload.deinitPtr(alloc),
            .@"catch" => |s| {
                s.lhs.deinitPtr(alloc);
                s.rhs.deinitPtr(alloc);
            },
        }
    }
};

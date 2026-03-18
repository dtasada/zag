const std = @import("std");
const utils = @import("utils");

const ast = @import("ast.zig");
const Statement = ast.Statement;
const Type = ast.Type;

pub const Expression = union(enum) {
    bad_node: struct { pos: utils.Position },

    // literals
    ident: struct { pos: utils.Position, payload: []const u8 },
    string: struct { pos: utils.Position, payload: []const u8 },
    char: struct { pos: utils.Position, payload: u8 },
    int: struct { pos: utils.Position, payload: u64 },
    float: struct { pos: utils.Position, payload: f64 },

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
    type: struct { pos: utils.Position, payload: Type },
    @"try": struct { pos: utils.Position, payload: *const Expression },
    @"catch": struct {
        pos: utils.Position,
        lhs: *const Expression,
        rhs: *const Expression,
    },

    pub const Match = struct {
        pub const Case = struct {
            pub const Condition = union(enum) {
                opts: []const Expression,
                @"else",
            };

            pos: utils.Position,
            condition: Condition,
            result: Statement,

            pub fn clone(self: Case, alloc: std.mem.Allocator) !Case {
                return .{
                    .pos = try self.pos.clone(alloc),
                    .condition = switch (self.condition) {
                        .opts => |opts| .{ .opts = try utils.cloneSlice(ast.Expression, opts, alloc) },
                        .@"else" => .@"else",
                    },
                    .result = try self.result.clone(alloc),
                };
            }

            pub fn deinit(self: Case, alloc: std.mem.Allocator) void {
                self.pos.deinit(alloc);
                if (self.condition == .opts) utils.deinitSlice(Expression, self.condition.opts, alloc);
                self.result.deinit(alloc);
            }
        };

        pos: utils.Position,
        condition: *const Expression,
        cases: []const Case,
    };

    pub const Block = struct { pos: utils.Position, payload: ast.Block };
    pub const Generic = struct {
        pos: utils.Position,
        lhs: *const Expression,
        arguments: ast.ArgumentList,
    };

    pub const Binary = struct {
        pos: utils.Position,
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

        pos: utils.Position,
        left: *const Expression,
        comparisons: []const Item,
    };

    pub const Member = struct {
        pos: utils.Position,
        parent: *const Expression,
        member_name: []const u8,
    };

    pub const Dereference = struct {
        pos: utils.Position,
        parent: *const Expression,
    };

    pub const Call = struct {
        pos: utils.Position,
        callee: *const Expression,
        args: ast.ArgumentList,
    };

    pub const Prefix = struct {
        pos: utils.Position,
        op: ast.PrefixOperator,
        rhs: *const Expression,
    };

    pub const Assignment = struct {
        pos: utils.Position,
        assignee: *const Expression,
        op: ast.AssignmentOperator,
        value: *const Expression,
    };

    pub const StructInstantiation = struct {
        pos: utils.Position,
        type_expr: *const Expression,
        members: *std.StringHashMap(Expression),
    };

    pub const ArrayInstantiation = struct {
        pos: utils.Position,
        length: *const Expression,
        type: Type,
        contents: []const Expression,
    };

    const Range = struct {
        pos: utils.Position,
        start: *const Expression,
        end: ?*const Expression,
        inclusive: bool,
    };

    const Reference = struct {
        pos: utils.Position,
        inner: *const Expression,
        is_mut: bool,
    };

    pub const If = struct {
        pos: utils.Position,
        condition: *const Expression,
        capture: ?utils.Capture,
        body: *const Expression,
        @"else": ?*const Expression = null,
    };

    pub const Index = struct {
        pos: utils.Position,
        lhs: *const Expression,
        index: *const Expression,
    };

    pub const Slice = struct {
        pos: utils.Position,
        lhs: *const Expression,
        start: ?*const Expression,
        end: ?*const Expression,
        inclusive: bool,
    };

    pub inline fn getPosition(self: *const Expression) utils.Position {
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
                .pos = try s.pos.clone(alloc),
                .payload = try alloc.dupe(u8, s.payload),
            }),
            inline .int, .float, .char => |n, t| @unionInit(Expression, @tagName(t), .{
                .pos = try n.pos.clone(alloc),
                .payload = n.payload,
            }),
            .bad_node => |bn| .{ .bad_node = .{ .pos = try bn.pos.clone(alloc) } },
            .block => |block| .{
                .block = .{
                    .pos = try block.pos.clone(alloc),
                    .payload = try utils.cloneSlice(Statement, block.payload, alloc),
                },
            },
            .generic => |generic| .{
                .generic = .{
                    .pos = try generic.pos.clone(alloc),
                    .lhs = try generic.lhs.clonePtr(alloc),
                    .arguments = try utils.cloneSlice(ast.Expression, generic.arguments, alloc),
                },
            },
            .binary => |binary| .{
                .binary = .{
                    .pos = try binary.pos.clone(alloc),
                    .lhs = try binary.lhs.clonePtr(alloc),
                    .op = binary.op,
                    .rhs = try binary.rhs.clonePtr(alloc),
                },
            },
            .comparison => |comparison| .{
                .comparison = .{
                    .pos = try comparison.pos.clone(alloc),
                    .left = try comparison.left.clonePtr(alloc),
                    .comparisons = try utils.cloneSlice(Comparison.Item, comparison.comparisons, alloc),
                },
            },
            .member => |member| .{
                .member = .{
                    .pos = try member.pos.clone(alloc),
                    .parent = try member.parent.clonePtr(alloc),
                    .member_name = try alloc.dupe(u8, member.member_name),
                },
            },
            .dereference => |dereference| .{
                .dereference = .{
                    .pos = try dereference.pos.clone(alloc),
                    .parent = try dereference.parent.clonePtr(alloc),
                },
            },
            .call => |call| .{
                .call = .{
                    .pos = try call.pos.clone(alloc),
                    .callee = try call.callee.clonePtr(alloc),
                    .args = try utils.cloneSlice(ast.Expression, call.args, alloc),
                },
            },
            .prefix => |prefix| .{
                .prefix = .{
                    .pos = try prefix.pos.clone(alloc),
                    .op = prefix.op,
                    .rhs = try prefix.rhs.clonePtr(alloc),
                },
            },
            .assignment => |assignment| .{
                .assignment = .{
                    .pos = try assignment.pos.clone(alloc),
                    .assignee = try assignment.assignee.clonePtr(alloc),
                    .op = assignment.op,
                    .value = try assignment.value.clonePtr(alloc),
                },
            },
            .struct_instantiation => |si| .{
                .struct_instantiation = .{
                    .pos = try si.pos.clone(alloc),
                    .type_expr = try si.type_expr.clonePtr(alloc),
                    .members = b: {
                        const new_map = try alloc.create(std.StringHashMap(Expression));
                        new_map.* = .init(alloc);
                        var it = si.members.iterator();
                        while (it.next()) |entry| try new_map.put(
                            try alloc.dupe(u8, entry.key_ptr.*),
                            try entry.value_ptr.*.clone(alloc),
                        );
                        break :b new_map;
                    },
                },
            },
            .array_instantiation => |ai| .{
                .array_instantiation = .{
                    .pos = try ai.pos.clone(alloc),
                    .length = try ai.length.clonePtr(alloc),
                    .type = try ai.type.clone(alloc),
                    .contents = try utils.cloneSlice(ast.Expression, ai.contents, alloc),
                },
            },
            .range => |range| .{
                .range = .{
                    .pos = try range.pos.clone(alloc),
                    .start = try range.start.clonePtr(alloc),
                    .end = if (range.end) |e| try e.clonePtr(alloc) else null,
                    .inclusive = range.inclusive,
                },
            },
            .reference => |reference| .{
                .reference = .{
                    .pos = try reference.pos.clone(alloc),
                    .inner = try reference.inner.clonePtr(alloc),
                    .is_mut = reference.is_mut,
                },
            },
            .@"if" => |@"if"| .{
                .@"if" = .{
                    .pos = try @"if".pos.clone(alloc),
                    .condition = try @"if".condition.clonePtr(alloc),
                    .capture = if (@"if".capture) |c| try c.clone(alloc) else null,
                    .body = try @"if".body.clonePtr(alloc),
                    .@"else" = if (@"if".@"else") |e| try e.clonePtr(alloc) else null,
                },
            },
            .index => |index| .{
                .index = .{
                    .pos = try index.pos.clone(alloc),
                    .lhs = try index.lhs.clonePtr(alloc),
                    .index = try index.index.clonePtr(alloc),
                },
            },
            .slice => |slice| .{
                .slice = .{
                    .pos = try slice.pos.clone(alloc),
                    .lhs = try slice.lhs.clonePtr(alloc),
                    .start = if (slice.start) |s| try s.clonePtr(alloc) else null,
                    .end = if (slice.end) |e| try e.clonePtr(alloc) else null,
                    .inclusive = slice.inclusive,
                },
            },
            .match => |match| .{
                .match = .{
                    .pos = try match.pos.clone(alloc),
                    .condition = try match.condition.clonePtr(alloc),
                    .cases = try utils.cloneSlice(Expression.Match.Case, match.cases, alloc),
                },
            },
            .type => |t| .{ .type = .{ .pos = try t.pos.clone(alloc), .payload = try t.payload.clone(alloc) } },
            .@"try" => |@"try"| .{
                .@"try" = .{
                    .pos = try @"try".pos.clone(alloc),
                    .payload = try @"try".payload.clonePtr(alloc),
                },
            },
            .@"catch" => |@"catch"| .{
                .@"catch" = .{
                    .pos = try @"catch".pos.clone(alloc),
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
            inline else => |s| s.pos.deinit(alloc),
        }

        switch (self) {
            .bad_node, .char, .int, .float => {},
            inline .ident, .string => |s| alloc.free(s.payload),
            .@"if" => |@"if"| {
                @"if".condition.deinitPtr(alloc);
                if (@"if".capture) |c| c.deinit(alloc);

                @"if".body.deinitPtr(alloc);
                if (@"if".@"else") |@"else"| @"else".deinitPtr(alloc);
            },
            .array_instantiation => |ai| {
                ai.length.deinitPtr(alloc);
                ai.type.deinit(alloc);
                utils.deinitSlice(Expression, ai.contents, alloc);
            },
            .assignment => |a| {
                a.assignee.deinitPtr(alloc);
                a.value.deinitPtr(alloc);
            },
            .binary => |bin| {
                bin.lhs.deinitPtr(alloc);
                bin.rhs.deinitPtr(alloc);
            },
            .block => |block| utils.deinitSlice(Statement, block.payload, alloc),
            .call => |call| {
                call.callee.deinitPtr(alloc);
                utils.deinitSlice(Expression, call.args, alloc);
            },
            .comparison => |cmp| {
                cmp.left.deinitPtr(alloc);
                utils.deinitSlice(Comparison.Item, cmp.comparisons, alloc);
            },
            .dereference => |deref| deref.parent.deinitPtr(alloc),
            .generic => |generic| {
                generic.lhs.deinitPtr(alloc);
                utils.deinitSlice(Expression, generic.arguments, alloc);
            },
            .index => |index| {
                index.lhs.deinitPtr(alloc);
                index.index.deinitPtr(alloc);
            },
            .slice => |slice| {
                slice.lhs.deinitPtr(alloc);
                if (slice.start) |start| start.deinitPtr(alloc);
                if (slice.end) |end| end.deinitPtr(alloc);
            },
            .match => |match| {
                match.condition.deinitPtr(alloc);
                utils.deinitSlice(Match.Case, match.cases, alloc);
            },
            .member => |member| {
                member.parent.deinitPtr(alloc);
                alloc.free(member.member_name);
            },
            .prefix => |prefix| prefix.rhs.deinitPtr(alloc),
            .range => |range| {
                range.start.deinitPtr(alloc);
                if (range.end) |end| end.deinitPtr(alloc);
            },
            .reference => |ref| ref.inner.deinitPtr(alloc),
            .struct_instantiation => |si| {
                si.type_expr.deinitPtr(alloc);
                var it = si.members.iterator();
                while (it.next()) |expr| expr.value_ptr.deinit(alloc);
                si.members.deinit();
                alloc.destroy(si.members);
            },
            .type => |t| t.payload.deinit(alloc),
            .@"try" => |t| t.payload.deinitPtr(alloc),
            .@"catch" => |c| {
                c.lhs.deinitPtr(alloc);
                c.rhs.deinitPtr(alloc);
            },
        }
    }
};

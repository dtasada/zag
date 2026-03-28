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
                const result = try self.result.clone(alloc);
                errdefer result.deinit(alloc);

                return .{
                    .pos = self.pos,
                    .condition = switch (self.condition) {
                        .opts => |opts| .{ .opts = try utils.cloneSlice(ast.Expression, opts, alloc) },
                        .@"else" => .@"else",
                    },
                    .result = result,
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
                const name = try alloc.dupe(u8, self.name);
                errdefer alloc.free(name);

                const value = try self.value.clone(alloc);
                errdefer value.deinit(alloc);

                return .{
                    .name = name,
                    .value = value,
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
            .generic => |generic| {
                const lhs = try generic.lhs.clonePtr(alloc);
                errdefer lhs.deinitPtr(alloc);

                const arguments = try utils.cloneSlice(ast.Expression, generic.arguments, alloc);
                errdefer utils.deinitSlice(ast.Expression, arguments, alloc);

                return .{
                    .generic = .{
                        .pos = generic.pos,
                        .lhs = lhs,
                        .arguments = arguments,
                    },
                };
            },
            .binary => |binary| {
                const lhs = try binary.lhs.clonePtr(alloc);
                errdefer lhs.deinitPtr(alloc);

                const rhs = try binary.rhs.clonePtr(alloc);
                errdefer rhs.deinitPtr(alloc);

                return .{
                    .binary = .{
                        .pos = binary.pos,
                        .lhs = lhs,
                        .op = binary.op,
                        .rhs = rhs,
                    },
                };
            },
            .comparison => |comparison| {
                const left = try comparison.left.clonePtr(alloc);
                errdefer left.deinitPtr(alloc);

                const comparisons = try utils.cloneSlice(Comparison.Item, comparison.comparisons, alloc);
                errdefer utils.deinitSlice(Comparison.Item, comparisons, alloc);

                return .{
                    .comparison = .{
                        .pos = comparison.pos,
                        .left = left,
                        .comparisons = comparisons,
                    },
                };
            },
            .member => |member| {
                const parent = try member.parent.clonePtr(alloc);
                errdefer parent.deinitPtr(alloc);

                const member_name = try alloc.dupe(u8, member.member_name);
                errdefer alloc.free(member_name);

                return .{
                    .member = .{
                        .pos = member.pos,
                        .parent = parent,
                        .member_name = member_name,
                    },
                };
            },
            .dereference => |dereference| .{
                .dereference = .{
                    .pos = dereference.pos,
                    .parent = try dereference.parent.clonePtr(alloc),
                },
            },
            .call => |call| {
                const callee = try call.callee.clonePtr(alloc);
                errdefer callee.deinitPtr(alloc);

                const args = try utils.cloneSlice(ast.Expression, call.args, alloc);
                errdefer utils.deinitSlice(ast.Expression, args, alloc);

                return .{
                    .call = .{
                        .pos = call.pos,
                        .callee = callee,
                        .args = args,
                    },
                };
            },
            .prefix => |prefix| .{
                .prefix = .{
                    .pos = prefix.pos,
                    .op = prefix.op,
                    .rhs = try prefix.rhs.clonePtr(alloc),
                },
            },
            .assignment => |assignment| {
                const assignee = try assignment.assignee.clonePtr(alloc);
                errdefer assignee.deinitPtr(alloc);

                const value = try assignment.value.clonePtr(alloc);
                errdefer value.deinitPtr(alloc);

                return .{
                    .assignment = .{
                        .pos = assignment.pos,
                        .assignee = assignee,
                        .op = assignment.op,
                        .value = value,
                    },
                };
            },
            .struct_instantiation => |si| {
                const type_expr = try si.type_expr.clonePtr(alloc);
                errdefer type_expr.deinitPtr(alloc);

                const members = try utils.cloneSlice(StructInstantiation.Member, si.members, alloc);
                errdefer utils.deinitSlice(StructInstantiation.Member, members, alloc);

                return .{
                    .struct_instantiation = .{
                        .pos = si.pos,
                        .type_expr = type_expr,
                        .members = members,
                    },
                };
            },
            .array_instantiation => |ai| {
                const length = try ai.length.clonePtr(alloc);
                errdefer length.deinitPtr(alloc);

                const t = try ai.type.clone(alloc);
                errdefer t.deinitPtr(alloc);

                const contents = try utils.cloneSlice(ast.Expression, ai.contents, alloc);
                errdefer utils.deinitSlice(ast.Expression, contents, alloc);

                return .{
                    .array_instantiation = .{
                        .pos = ai.pos,
                        .length = length,
                        .type = t,
                        .contents = contents,
                    },
                };
            },
            .range => |range| {
                const start = try range.start.clonePtr(alloc);
                errdefer start.deinitPtr(alloc);

                const end = if (range.end) |e| try e.clonePtr(alloc) else null;
                errdefer if (range.end) |e| e.deinitPtr(alloc);

                return .{
                    .range = .{
                        .pos = range.pos,
                        .start = start,
                        .end = end,
                        .inclusive = range.inclusive,
                    },
                };
            },
            .reference => |reference| .{
                .reference = .{
                    .pos = reference.pos,
                    .inner = try reference.inner.clonePtr(alloc),
                    .is_mut = reference.is_mut,
                },
            },
            .@"if" => |@"if"| {
                const condition = try @"if".condition.clonePtr(alloc);
                errdefer condition.deinitPtr(alloc);

                const capture = if (@"if".capture) |c| try c.clone(alloc) else null;
                errdefer if (@"if".capture) |c| c.deinit(alloc);

                const body = try @"if".body.clonePtr(alloc);
                errdefer body.deinitPtr(alloc);

                const @"else" = try @"if".@"else".clonePtr(alloc);
                errdefer @"else".deinitPtr(alloc);

                return .{
                    .@"if" = .{
                        .pos = @"if".pos,
                        .condition = condition,
                        .capture = capture,
                        .body = body,
                        .@"else" = @"else",
                    },
                };
            },
            .index => |index| {
                const lhs = try index.lhs.clonePtr(alloc);
                errdefer lhs.deinitPtr(alloc);

                const i = try index.index.clonePtr(alloc);
                errdefer i.deinitPtr(alloc);

                return .{
                    .index = .{
                        .pos = index.pos,
                        .lhs = lhs,
                        .index = i,
                    },
                };
            },
            .slice => |slice| {
                const lhs = try slice.lhs.clonePtr(alloc);
                errdefer lhs.deinitPtr(alloc);

                const start = try slice.start.clonePtr(alloc);
                errdefer start.deinitPtr(alloc);

                const end = if (slice.end) |e| try e.clonePtr(alloc) else null;
                errdefer if (end) |e| e.deinitPtr(alloc);

                return .{
                    .slice = .{
                        .pos = slice.pos,
                        .lhs = lhs,
                        .start = start,
                        .end = end,
                        .inclusive = slice.inclusive,
                    },
                };
            },
            .match => |match| {
                const condition = try match.condition.clonePtr(alloc);
                errdefer condition.deinitPtr(alloc);

                const cases = try utils.cloneSlice(Expression.Match.Case, match.cases, alloc);
                errdefer utils.deinitSlice(Expression.Match.Case, cases, alloc);

                return .{
                    .match = .{
                        .pos = match.pos,
                        .condition = condition,
                        .cases = cases,
                    },
                };
            },
            .type => |t| .{ .type = .{ .pos = t.pos, .payload = try t.payload.clone(alloc) } },
            .@"try" => |@"try"| .{
                .@"try" = .{
                    .pos = @"try".pos,
                    .payload = try @"try".payload.clonePtr(alloc),
                },
            },
            .@"catch" => |@"catch"| {
                const lhs = try @"catch".lhs.clonePtr(alloc);
                errdefer lhs.deinitPtr(alloc);

                const rhs = try @"catch".rhs.clonePtr(alloc);
                errdefer rhs.deinitPtr(alloc);

                return .{
                    .@"catch" = .{
                        .pos = @"catch".pos,
                        .lhs = lhs,
                        .rhs = rhs,
                    },
                };
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

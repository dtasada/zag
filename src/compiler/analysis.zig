const std = @import("std");
const ast = @import("ast");
const Compiler = @import("Compiler.zig");
const Type = @import("types/Type.zig").Type;

pub fn blockReturns(block: ast.Block) bool {
    for (block) |stmt| {
        if (statementReturns(stmt)) {
            // This statement guarantees a return, so any code after it is
            // effectively unreachable from a path-checking perspective.
            // The block as a whole returns.
            return true;
        }
    }

    // If we finished the loop, no statement guaranteed a return.
    return false;
}

pub fn statementReturns(statement: ast.Statement) bool {
    return switch (statement) {
        .@"return", .block_eval => true,

        .@"if" => |if_stmt| blk: {
            if (if_stmt.@"else") |else_stmt| {
                if (statementReturns(if_stmt.body.*) and statementReturns(else_stmt.*)) {
                    break :blk true;
                }
            }
            break :blk false;
        },

        .block => |block_stmt| blockReturns(block_stmt.payload),

        .expression => |expr| switch (expr) {
            .match => |match_expr| blk: {
                var has_else = false;
                if (match_expr.cases.len == 0) {
                    break :blk false;
                }

                for (match_expr.cases) |case| {
                    if (case.condition == .@"else") {
                        has_else = true;
                    }
                    if (!statementReturns(case.result)) {
                        break :blk false;
                    }
                }
                break :blk has_else;
            },
            .block => |block_expr| blockReturns(block_expr.payload),
            else => false,
        },

        .@"while" => |while_stmt| blk: {
            if (while_stmt.condition == .ident) {
                if (std.mem.eql(u8, while_stmt.condition.ident.payload, "true")) {
                    break :blk statementReturns(while_stmt.body.*);
                }
            }
            break :blk false;
        },

        else => false,
    };
}

pub fn topoVisit(self: *Compiler, idx: usize, visited: []u8, order: *std.ArrayList(usize)) !void {
    visited[idx] = 1;

    const t = self.type_def_blocks.items[idx].type;
    var deps: std.ArrayList(Type) = .empty;
    defer deps.deinit(self.alloc);
    try self.collectTypeDeps(t, &deps);

    for (deps.items) |dep| {
        for (self.type_def_blocks.items, 0..) |*block, j| {
            if (typeDefsMatch(dep, block.type)) {
                if (visited[j] == 0) try topoVisit(self, j, visited, order);
                break;
            }
        }
    }

    visited[idx] = 2;
    try order.append(self.alloc, idx);
}

fn typeDefsMatch(a: Type, b: Type) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .@"struct" => |s| std.mem.eql(u8, s.inner_name, b.@"struct".inner_name),
        .@"union" => |u| std.mem.eql(u8, u.inner_name, b.@"union".inner_name),
        .@"enum" => |e| std.mem.eql(u8, e.inner_name, b.@"enum".inner_name),
        .error_union => |eu| typeDefsMatch(eu.success.*, b.error_union.success.*) and
            typeDefsMatch(eu.failure.*, b.error_union.failure.*),
        .slice => |sa| typeDefsMatch(sa.inner.*, b.slice.inner.*),
        .optional => |oa| typeDefsMatch(oa.*, b.optional.*),
        .array => |aa| typeDefsMatch(aa.inner.*, b.array.inner.*) and aa.size == b.array.size,
        else => true,
    };
}

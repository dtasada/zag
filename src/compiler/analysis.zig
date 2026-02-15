const std = @import("std");
const ast = @import("Parser").ast;

pub fn blockReturns(block: ast.Block) bool {
    for (block.items) |stmt| {
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

        .block => |block_stmt| blockReturns(block_stmt.block),

        .expression => |expr| switch (expr) {
            .match => |match_expr| blk: {
                var has_else = false;
                if (match_expr.cases.items.len == 0) {
                    break :blk false;
                }

                for (match_expr.cases.items) |case| {
                    if (case.condition == .@"else") {
                        has_else = true;
                    }
                    if (!statementReturns(case.result)) {
                        break :blk false;
                    }
                }
                break :blk has_else;
            },
            .block => |block_expr| blockReturns(block_expr.block),
            else => false,
        },

        .@"while" => |while_stmt| blk: {
            if (while_stmt.condition == .ident) {
                if (std.mem.eql(u8, while_stmt.condition.ident.ident, "true")) {
                    break :blk statementReturns(while_stmt.body.*);
                }
            }
            break :blk false;
        },

        else => false,
    };
}

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
        .@"return" => true,

        .@"if" => |if_stmt| {
            // An if statement only guarantees a return if it has an else
            // branch AND both branches are guaranteed to return.
            if (if_stmt.@"else") |else_stmt| {
                return statementReturns(if_stmt.body.*) and statementReturns(else_stmt.*);
            }
            return false;
        },

        .block => |block_stmt| return blockReturns(block_stmt.block),

        // For now, assume loops and other statements don't guarantee a return.
        else => false,
    };
}

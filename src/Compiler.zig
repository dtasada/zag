const std = @import("std");
const llvm = @import("llvm");

const ast = @import("parser/ast.zig");

const Self = @This();

pub const CompilerError = error{
    FailedToCreateModule,
    UnsupportedType,
    UnsupportedExpression,
    UndeclaredVariable,
    OutOfMemory,
    AssignmentToImmutableVariable,
};

const Symbol = union(enum) {
    parameter: struct {
        val: llvm.types.LLVMValueRef,
        ty: llvm.types.LLVMTypeRef,
    },
    local: struct {
        ptr: llvm.types.LLVMValueRef,
        ty: llvm.types.LLVMTypeRef,
        is_mut: bool,
    },
};

alloc: std.mem.Allocator,
context: llvm.types.LLVMContextRef,
builder: llvm.types.LLVMBuilderRef,
module: llvm.types.LLVMModuleRef,
variables: std.StringHashMap(Symbol),

pub fn init(alloc: std.mem.Allocator) !*Self {
    const self = try alloc.create(Self);

    const context = llvm.core.LLVMContextCreate();
    const module = llvm.core.LLVMModuleCreateWithNameInContext("main", context) orelse return error.FailedToCreateModule;
    const builder = llvm.core.LLVMCreateBuilderInContext(context);

    self.* = .{
        .alloc = alloc,
        .context = context,
        .builder = builder,
        .module = module,
        .variables = .init(alloc),
    };

    return self;
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    self.variables.deinit();
    llvm.core.LLVMDisposeBuilder(self.builder);
    llvm.core.LLVMDisposeModule(self.module);
    llvm.core.LLVMContextDispose(self.context);
    alloc.destroy(self);
}

/// Entry point for the compiler. Prints LLVM IR to stdout
pub fn emit(self: *Self, root: ast.RootNode) CompilerError!void {
    for (root.items) |statement| {
        try self.compileStatement(statement);
    }

    // print IR
    llvm.core.LLVMDumpModule(self.module);
}

fn compileStatement(self: *Self, statement: ast.Statement) CompilerError!void {
    switch (statement) {
        .function_definition => |fn_def| try self.compileFunctionDefinition(fn_def),
        .@"return" => |return_expr| try self.compileReturnStatement(return_expr),
        .variable_declaration => |var_decl| try self.compileVariableDeclaration(var_decl),
        .expression => |expr| _ = try self.compileExpression(expr),
        .@"if" => |if_stmt| try self.compileIfStatement(if_stmt),
        .@"while" => |while_stmt| try self.compileWhileStatement(while_stmt),
        .block => |block| try self.compileBlock(block),
        else => {},
    }
}

fn compileFunctionDefinition(self: *Self, fn_def: ast.FunctionDefinition) CompilerError!void {
    const fn_name = try self.cString(fn_def.name);

    const return_type = try self.compileType(fn_def.return_type);

    var param_types = try std.ArrayList(llvm.types.LLVMTypeRef).initCapacity(self.alloc, fn_def.parameters.items.len);
    defer param_types.deinit(self.alloc);
    for (fn_def.parameters.items) |param| {
        try param_types.append(self.alloc, try self.compileType(param.type));
    }

    const fn_type = llvm.core.LLVMFunctionType(return_type, param_types.items.ptr, @as(u32, @intCast(param_types.items.len)), 0);
    const fn_val = llvm.core.LLVMAddFunction(self.module, fn_name, fn_type);

    const entry = llvm.core.LLVMAppendBasicBlockInContext(self.context, fn_val, "entry");
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, entry);

    self.variables.clearRetainingCapacity();
    for (fn_def.parameters.items, 0..) |param, i| {
        const param_val = llvm.core.LLVMGetParam(fn_val, @as(u32, @intCast(i)));
        const param_name = try self.cString(param.name);
        llvm.core.LLVMSetValueName(param_val, param_name);
        const param_type = param_types.items[i];
        try self.variables.put(param.name, .{ .parameter = .{ .val = param_val, .ty = param_type } });
    }

    try self.compileBlock(fn_def.body);
}

fn compileBlock(self: *Self, block: ast.Block) CompilerError!void {
    for (block.items) |statement| {
        try self.compileStatement(statement);
    }
}

fn compileVariableDeclaration(self: *Self, var_decl: ast.Statement.VariableDeclaration) CompilerError!void {
    const value = try self.compileExpression(var_decl.assigned_value);
    const value_type = llvm.core.LLVMTypeOf(value);

    const var_name = try self.cString(var_decl.variable_name);
    const alloca = llvm.core.LLVMBuildAlloca(self.builder, value_type, var_name);
    _ = llvm.core.LLVMBuildStore(self.builder, value, alloca);

    try self.variables.put(var_decl.variable_name, .{ .local = .{ .ptr = alloca, .ty = value_type, .is_mut = var_decl.is_mut } });
}

fn compileReturnStatement(self: *Self, return_expr: ?ast.Expression) CompilerError!void {
    if (return_expr) |re| {
        const compiled_value = try self.compileExpression(re);
        _ = llvm.core.LLVMBuildRet(self.builder, compiled_value);
    } else _ = llvm.core.LLVMBuildRetVoid(self.builder);
}

fn compileExpression(self: *Self, expr: ast.Expression) CompilerError!llvm.types.LLVMValueRef {
    return switch (expr) {
        .uint => |uint| {
            const i32_type = llvm.core.LLVMInt32TypeInContext(self.context);
            return llvm.core.LLVMConstInt(i32_type, uint, 0);
        },
        .ident => |ident| {
            const symbol = self.variables.get(ident) orelse return error.UndeclaredVariable;
            return switch (symbol) {
                .parameter => |p| p.val,
                .local => |l| {
                    const variable_name = try self.cString(ident);
                    const pointee_type = l.ty;
                    return llvm.core.LLVMBuildLoad2(self.builder, pointee_type, l.ptr, variable_name);
                },
            };
        },
        .assignment => |ass| {
            // This logic needs to be more robust for complex assignees like member access.
            if (ass.assignee.* != .ident) {
                return error.UnsupportedExpression;
            }
            const var_name = ass.assignee.*.ident;
            const symbol = self.variables.get(var_name) orelse return error.UndeclaredVariable;

            const l = switch (symbol) {
                .local => |local| local,
                .parameter => return error.AssignmentToImmutableVariable,
            };

            if (!l.is_mut) {
                return error.AssignmentToImmutableVariable;
            }

            const rhs_value = try self.compileExpression(ass.value.*);

            const final_value = switch (ass.op) {
                .equals => rhs_value,
                .minus_equals => blk: {
                    const var_name_c = try self.cString(var_name);
                    const current_val = llvm.core.LLVMBuildLoad2(self.builder, l.ty, l.ptr, var_name_c);
                    const new_val = llvm.core.LLVMBuildSub(self.builder, current_val, rhs_value, "subtmp");
                    break :blk new_val;
                },
                // TODO: Add other compound assignment operators
                else => return error.UnsupportedExpression,
            };

            _ = llvm.core.LLVMBuildStore(self.builder, final_value, l.ptr);
            return final_value;
        },
        .binary => |bin_expr| {
            const lhs = try self.compileExpression(bin_expr.lhs.*);
            const rhs = try self.compileExpression(bin_expr.rhs.*);

            return switch (bin_expr.op) {
                .plus => llvm.core.LLVMBuildAdd(self.builder, lhs, rhs, "addtmp"),
                .dash => llvm.core.LLVMBuildSub(self.builder, lhs, rhs, "subtmp"),
                .asterisk => llvm.core.LLVMBuildMul(self.builder, lhs, rhs, "multmp"),
                .slash => llvm.core.LLVMBuildSDiv(self.builder, lhs, rhs, "divtmp"),
                .percent => llvm.core.LLVMBuildSRem(self.builder, lhs, rhs, "remtmp"),

                // Comparisons
                .equals_equals => llvm.core.LLVMBuildICmp(self.builder, .LLVMIntEQ, lhs, rhs, "eqtmp"),
                .bang_equals => llvm.core.LLVMBuildICmp(self.builder, .LLVMIntNE, lhs, rhs, "netmp"),
                .less => llvm.core.LLVMBuildICmp(self.builder, .LLVMIntSLT, lhs, rhs, "lttmp"),
                .less_equals => llvm.core.LLVMBuildICmp(self.builder, .LLVMIntSLE, lhs, rhs, "letmp"),
                .greater => llvm.core.LLVMBuildICmp(self.builder, .LLVMIntSGT, lhs, rhs, "gttmp"),
                .greater_equals => llvm.core.LLVMBuildICmp(self.builder, .LLVMIntSGE, lhs, rhs, "getmp"),

                // Bitwise
                .ampersand => llvm.core.LLVMBuildAnd(self.builder, lhs, rhs, "andtmp"),
                .pipe => llvm.core.LLVMBuildOr(self.builder, lhs, rhs, "ortmp"),
                .caret => llvm.core.LLVMBuildXor(self.builder, lhs, rhs, "xortmp"),
                .shift_left => llvm.core.LLVMBuildShl(self.builder, lhs, rhs, "shltmp"),
                .shift_right => llvm.core.LLVMBuildAShr(self.builder, lhs, rhs, "ashrtmp"),

                else => error.UnsupportedExpression,
            };
        },
        else => error.UnsupportedExpression,
    };
}

fn compileWhileStatement(self: *Self, while_stmt: ast.Statement.While) !void {
    // Get the function we're currently building in.
    const current_fn = llvm.core.LLVMGetBasicBlockParent(llvm.core.LLVMGetInsertBlock(self.builder));

    // Create the basic blocks for the loop structure.
    const cond_block = llvm.core.LLVMAppendBasicBlockInContext(self.context, current_fn, "while_cond");
    const loop_block = llvm.core.LLVMAppendBasicBlockInContext(self.context, current_fn, "while_loop");
    const after_block = llvm.core.LLVMAppendBasicBlockInContext(self.context, current_fn, "after_while");

    // From the current block, jump to the condition check.
    _ = llvm.core.LLVMBuildBr(self.builder, cond_block);

    // --- Condition Block ---
    // In this block, we evaluate the loop condition.
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, cond_block);

    const condition_bool = try self.compileExpression(while_stmt.condition.*);

    // Branch to the loop body or exit the loop.
    _ = llvm.core.LLVMBuildCondBr(self.builder, condition_bool, loop_block, after_block);

    // --- Loop Body Block ---
    // This block contains the code that runs on each iteration of the loop.
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, loop_block);

    // Compile the body of the loop.
    try self.compileStatement(while_stmt.body.*);

    // After the body, jump back to the condition check.
    _ = llvm.core.LLVMBuildBr(self.builder, cond_block);

    // --- Continue ---
    // Any subsequent code will be generated in the after_block.
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, after_block);
}

fn compileIfStatement(self: *Self, if_stmt: ast.Statement.If) !void {
    // First, we compile the condition expression.
    const condition_bool = try self.compileExpression(if_stmt.condition.*);

    // Now we need the function that we are currently building, so we can add blocks to it.
    const current_fn = llvm.core.LLVMGetBasicBlockParent(llvm.core.LLVMGetInsertBlock(self.builder));

    // Create the basic blocks for the then, else, and merge branches.
    const then_block = llvm.core.LLVMAppendBasicBlockInContext(self.context, current_fn, "then");
    const else_block = llvm.core.LLVMAppendBasicBlockInContext(self.context, current_fn, "else");
    const merge_block = llvm.core.LLVMAppendBasicBlockInContext(self.context, current_fn, "ifcont");

    // Create the conditional branch.
    _ = llvm.core.LLVMBuildCondBr(self.builder, condition_bool, then_block, else_block);

    // --- Then Block ---
    // Move the builder to the 'then' block to start inserting instructions there.
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, then_block);

    // Compile the body of the 'then' block.
    _ = try self.compileStatement(if_stmt.body.*);

    // After the 'then' block is done, we need to jump to the 'merge' block.
    _ = llvm.core.LLVMBuildBr(self.builder, merge_block);

    // --- Else Block ---
    // Move the builder to the 'else' block.
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, else_block);

    // Compile the body of the 'else' block, if it exists.
    if (if_stmt.@"else") |else_expr| {
        _ = try self.compileStatement(else_expr.*);
    }

    // After the 'else' block is done, we also need to jump to the 'merge' block.
    _ = llvm.core.LLVMBuildBr(self.builder, merge_block);

    // --- Continue ---
    // Finally, move the builder to the 'merge' block so that any code after
    // the if/else statement is generated correctly.
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, merge_block);
}

fn compileType(self: *Self, ast_type: ast.Type) !llvm.types.LLVMTypeRef {
    return switch (ast_type) {
        .symbol => |sym| if (std.mem.eql(u8, sym, "void"))
            llvm.core.LLVMVoidTypeInContext(self.context)
        else if (std.mem.eql(u8, sym, "i8"))
            llvm.core.LLVMInt8TypeInContext(self.context)
        else if (std.mem.eql(u8, sym, "i16"))
            llvm.core.LLVMInt16TypeInContext(self.context)
        else if (std.mem.eql(u8, sym, "i32"))
            llvm.core.LLVMInt32TypeInContext(self.context)
        else if (std.mem.eql(u8, sym, "i64"))
            llvm.core.LLVMInt64TypeInContext(self.context)
        else
            error.UnsupportedType,
        else => error.UnsupportedType,
    };
}

inline fn cString(self: *const Self, s: []const u8) ![*:0]const u8 {
    return try std.mem.concatWithSentinel(self.alloc, u8, &.{s}, 0);
}

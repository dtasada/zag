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
};

alloc: std.mem.Allocator,
context: llvm.types.LLVMContextRef,
builder: llvm.types.LLVMBuilderRef,
module: llvm.types.LLVMModuleRef,
variables: std.StringHashMap(llvm.types.LLVMValueRef),

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
        else => {},
    }
}

fn compileFunctionDefinition(self: *Self, fn_def: ast.FunctionDefinition) CompilerError!void {
    // Create a null-terminated C string for the function name
    const fn_name: [*:0]u8 = try std.mem.concatWithSentinel(self.alloc, u8, &.{fn_def.name}, 0);

    // Compile types
    const return_type = try self.compileType(fn_def.return_type);

    var param_types = try std.ArrayList(llvm.types.LLVMTypeRef).initCapacity(self.alloc, fn_def.parameters.items.len);
    defer param_types.deinit(self.alloc);
    for (fn_def.parameters.items) |param| {
        try param_types.append(self.alloc, try self.compileType(param.type));
    }

    // Create function type and add it to the module
    const fn_type = llvm.core.LLVMFunctionType(return_type, param_types.items.ptr, @as(u32, @intCast(param_types.items.len)), 0);
    const fn_val = llvm.core.LLVMAddFunction(self.module, fn_name, fn_type);

    // Create entry basic block
    const entry = llvm.core.LLVMAppendBasicBlockInContext(self.context, fn_val, "entry");
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, entry);

    // Create a new scope for the function's variables
    self.variables.clearRetainingCapacity();
    for (fn_def.parameters.items, 0..) |param, i| {
        const param_val = llvm.core.LLVMGetParam(fn_val, @as(u32, @intCast(i)));
        const param_name: [*:0]const u8 = try std.mem.concatWithSentinel(self.alloc, u8, &.{param.name}, 0);
        llvm.core.LLVMSetValueName(param_val, param_name);
        try self.variables.put(param.name, param_val);
    }

    // Compile the function body
    try self.compileBlock(fn_def.body);
}

fn compileBlock(self: *Self, block: ast.Block) CompilerError!void {
    for (block.items) |statement| {
        try self.compileStatement(statement);
    }
}

fn compileReturnStatement(self: *Self, return_expr: ast.Expression) CompilerError!void {
    const compiled_value = try self.compileExpression(return_expr);
    _ = llvm.core.LLVMBuildRet(self.builder, compiled_value);
}

fn compileExpression(self: *Self, expr: ast.Expression) !llvm.types.LLVMValueRef {
    return switch (expr) {
        .uint => |int_lit| {
            const i32_type = llvm.core.LLVMInt32TypeInContext(self.context);
            return llvm.core.LLVMConstInt(i32_type, int_lit, 0);
        },
        .ident => |name| self.variables.get(name) orelse error.UndeclaredVariable,
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

fn compileType(self: *Self, ast_type: ast.Type) !llvm.types.LLVMTypeRef {
    return switch (ast_type) {
        .symbol => |sym| {
            if (std.mem.eql(u8, sym, "void")) {
                return llvm.core.LLVMVoidTypeInContext(self.context);
            } else if (std.mem.eql(u8, sym, "i32")) {
                return llvm.core.LLVMInt32TypeInContext(self.context);
            } else {
                return error.UnsupportedType;
            }
        },
        else => error.UnsupportedType,
    };
}

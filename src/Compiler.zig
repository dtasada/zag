const std = @import("std");
const llvm = @import("llvm");

const utils = @import("utils.zig");
const ast = @import("parser/ast.zig");

const Parser = @import("parser/Parser.zig");

const Self = @This();

pub const CompilerError = error{
    FailedToCreateModule,
    UnsupportedType,
    UnsupportedExpression,
    UndeclaredVariable,
    UndeclaredType,
    UndeclaredField,
    VariableRedeclaration,
    OutOfMemory,
    AssignmentToImmutableVariable,
    MemberExpressionOnPrimitiveType,
} || Parser.ParserError;

const UserDefinedType = struct {
    llvm_type: llvm.types.LLVMTypeRef,
    fields: std.StringHashMap(u32), // name -> index
};

const Symbol = union(enum) {
    parameter: struct {
        val: llvm.types.LLVMValueRef,
        ty: llvm.types.LLVMTypeRef,
        is_mut: bool, // Added is_mut field
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
variables: std.ArrayList(std.StringHashMap(Symbol)) = .{},
types: std.StringHashMap(*UserDefinedType),
current_self_type: ?llvm.types.LLVMTypeRef = null,
parser: *const Parser,

pub fn init(alloc: std.mem.Allocator, parser: *const Parser) !*Self {
    const self = try alloc.create(Self);

    const context = llvm.core.LLVMContextCreate();
    const module = llvm.core.LLVMModuleCreateWithNameInContext("main", context) orelse
        return error.FailedToCreateModule;
    const builder = llvm.core.LLVMCreateBuilderInContext(context);

    self.* = .{
        .alloc = alloc,
        .context = context,
        .builder = builder,
        .module = module,
        .types = std.StringHashMap(*UserDefinedType).init(alloc),
        .parser = parser,
    };

    try self.variables.append(self.alloc, std.StringHashMap(Symbol).init(alloc)); // global scope

    return self;
}

pub fn deinit(self: *Self) void {
    for (self.variables.items) |*scope|
        scope.deinit();

    self.variables.deinit(self.alloc);

    var iter = self.types.iterator();
    while (iter.next()) |entry| {
        entry.value_ptr.*.fields.deinit();
        self.alloc.destroy(entry.value_ptr.*);
    }
    self.types.deinit();

    llvm.core.LLVMDisposeBuilder(self.builder);
    llvm.core.LLVMDisposeModule(self.module);
    llvm.core.LLVMContextDispose(self.context);
}

/// Entry point for the compiler. Compiles AST into LLVM module.
pub fn emit(self: *Self) !void {
    for (self.parser.output.items) |statement|
        try self.compileStatement(statement);

    // Initialize LLVM targets
    llvm.target.LLVMInitializeAllTargetInfos();
    llvm.target.LLVMInitializeAllTargets();
    llvm.target.LLVMInitializeAllTargetMCs();
    llvm.target.LLVMInitializeAllAsmParsers();
    llvm.target.LLVMInitializeAllAsmPrinters();

    const triple = llvm.target_machine.LLVMGetDefaultTargetTriple();
    var target: llvm.types.LLVMTargetRef = undefined;
    var err_target: [*c]u8 = null;
    if (llvm.target_machine.LLVMGetTargetFromTriple(triple, &target, &err_target) != 0) {
        utils.print("Failed to get target from triple: {s}\n", .{err_target}, .red);
        return;
    }

    const target_machine = llvm.target_machine.LLVMCreateTargetMachine(
        target,
        triple,
        "generic",
        "",
        .LLVMCodeGenLevelAggressive,
        .LLVMRelocPIC,
        .LLVMCodeModelDefault,
    );

    llvm.target.LLVMSetModuleDataLayout(self.module, llvm.target_machine.LLVMCreateTargetDataLayout(target_machine));

    try std.fs.cwd().makePath(".dmr-out");
    const object_file = try std.fs.cwd().createFile(".dmr-out/main.o", .{});
    defer object_file.close();

    const out_path = try std.fs.path.join(self.alloc, &.{ std.fs.cwd().realpathAlloc(self.alloc, ".") catch unreachable, ".dmr-out/main.o" });
    defer self.alloc.free(out_path);

    const out_path_c = try self.cString(out_path);

    var err: [*c]u8 = null;
    if (llvm.target_machine.LLVMTargetMachineEmitToFile(
        target_machine,
        self.module,
        out_path_c,
        .LLVMObjectFile,
        &err,
    ) != 0) {
        utils.print("Failed to emit object file: {s}\n", .{err}, .red);
        llvm.core.LLVMDisposeMessage(err); // Free if not null
        return error.CompilerFailed;
    }
}

// helper functions

/// finds variable in closest possible scope.
fn findVariable(self: *const Self, name: []const u8) ?Symbol {
    for (self.variables.items, 0..) |_, i| {
        const scope_idx = self.variables.items.len - 1 - i;
        if (self.variables.items[scope_idx].get(name)) |symbol| {
            return symbol;
        }
    }

    return null;
}

fn addVariable(self: *Self, name: []const u8, symbol: Symbol) !void {
    const last_scope_idx = self.variables.items.len - 1;
    try self.variables.items[last_scope_idx].put(name, symbol);
}

fn createEntryBlockAlloca(self: *Self, @"fn": llvm.types.LLVMValueRef, ty: llvm.types.LLVMTypeRef, name: [*:0]const u8) llvm.types.LLVMValueRef {
    const builder = llvm.core.LLVMCreateBuilderInContext(self.context);
    defer llvm.core.LLVMDisposeBuilder(builder);

    const entry_block = llvm.core.LLVMGetEntryBasicBlock(@"fn");
    const first_instruction = llvm.core.LLVMGetFirstInstruction(entry_block);

    if (first_instruction) |instr| {
        llvm.core.LLVMPositionBuilderBefore(builder, instr);
    } else {
        llvm.core.LLVMPositionBuilderAtEnd(builder, entry_block);
    }
    return llvm.core.LLVMBuildAlloca(builder, ty, name);
}

fn compileStatement(self: *Self, statement: ast.Statement) CompilerError!void {
    switch (statement) {
        .function_definition => |fn_def| try self.compileFunctionDefinition(fn_def),
        .struct_declaration => |struct_decl| try self.compileStructDeclaration(struct_decl),
        .@"return" => |return_expr| try self.compileReturnStatement(return_expr),
        .variable_declaration => |_| try self.compileVariableDeclaration(statement),
        .expression => |expr| _ = try self.compileExpression(expr),
        .@"if" => |if_stmt| try self.compileIfStatement(if_stmt),
        .@"while" => |while_stmt| try self.compileWhileStatement(while_stmt),
        .@"for" => |for_stmt| try self.compileForStatement(for_stmt),
        .block => |block| try self.compileBlock(block),
        else => |other| std.debug.panic("panic: unimplemented statement {s}\n", .{@tagName(other)}),
    }
}

fn compileStructDeclaration(self: *Self, struct_decl: ast.Statement.StructDeclaration) !void {
    const struct_name_c = try self.cString(struct_decl.name);
    defer self.alloc.free(std.mem.span(struct_name_c));
    const struct_type_ref = llvm.core.LLVMStructCreateNamed(self.context, struct_name_c);

    const user_type = try self.alloc.create(UserDefinedType);
    user_type.* = .{
        .llvm_type = struct_type_ref,
        .fields = std.StringHashMap(u32).init(self.alloc),
    };

    try self.types.put(struct_decl.name, user_type);

    var field_types = try std.ArrayList(llvm.types.LLVMTypeRef).initCapacity(self.alloc, struct_decl.members.items.len);
    defer field_types.deinit(self.alloc);

    for (struct_decl.members.items, 0..) |field, i| {
        try field_types.append(self.alloc, try self.compileType(field.type));
        try user_type.fields.put(field.name, cUint(i));
    }

    llvm.core.LLVMStructSetBody(struct_type_ref, field_types.items.ptr, cUint(field_types.items.len), 0);

    // Method compilation must happen *after* the struct body is set.
    self.current_self_type = struct_type_ref;
    defer self.current_self_type = null;

    for (struct_decl.methods.items) |method| {
        const mangled_name = try std.fmt.allocPrint(self.alloc, "__MANGLE_{s}_{s}", .{ struct_decl.name, method.name });
        defer self.alloc.free(mangled_name);

        var method_with_mangled_name = method;
        method_with_mangled_name.name = mangled_name;

        // We pass the struct type itself as the self_param, which will be a pointer to the struct.

        try self.compileFunctionDefinition(method_with_mangled_name);
    }
}

pub fn compileFunctionDefinition(self: *Self, fn_def: ast.FunctionDefinition) CompilerError!void {
    const fn_name = try self.cString(fn_def.name);
    defer self.alloc.free(std.mem.span(fn_name));

    const return_type = try self.compileType(fn_def.return_type);

    var param_types = try std.ArrayList(llvm.types.LLVMTypeRef).initCapacity(
        self.alloc,
        fn_def.parameters.items.len,
    );
    defer param_types.deinit(self.alloc);

    for (fn_def.parameters.items) |param|
        try param_types.append(self.alloc, try self.compileType(param.type));

    const fn_type = llvm.core.LLVMFunctionType(return_type, param_types.items.ptr, cUint(param_types.items.len), 0);
    const fn_val = llvm.core.LLVMAddFunction(self.module, fn_name, fn_type);

    const entry = llvm.core.LLVMAppendBasicBlockInContext(self.context, fn_val, "entry");
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, entry);

    try self.variables.append(self.alloc, std.StringHashMap(Symbol).init(self.alloc));

    for (fn_def.parameters.items, 0..) |param, i| {
        const param_val = llvm.core.LLVMGetParam(fn_val, cUint(i));
        const param_name = try self.cString(param.name);
        llvm.core.LLVMSetValueName(param_val, param_name);
        const param_type = param_types.items[cUint(i)];
        const is_param_mut = switch (param.type) {
            .reference => |ref| ref.is_mut,
            else => false,
        };
        try self.addVariable(param.name, .{ .parameter = .{ .val = param_val, .ty = param_type, .is_mut = is_param_mut } });
    }

    try self.compileBlock(fn_def.body);

    // If the function returns void and the last block doesn't have a terminator,
    // add a `ret void` instruction.
    const last_block = llvm.core.LLVMGetInsertBlock(self.builder);
    const last_instr = llvm.core.LLVMGetLastInstruction(last_block);
    const has_terminator = if (last_instr) |instr| llvm.core.LLVMIsATerminatorInst(instr) != null else false;

    if (!has_terminator and llvm.core.LLVMGetTypeKind(return_type) == .LLVMVoidTypeKind) {
        _ = llvm.core.LLVMBuildRetVoid(self.builder);
    }

    var last_scope = self.variables.pop().?;
    last_scope.deinit();
}

fn compileBlock(self: *Self, block: ast.Block) CompilerError!void {
    try self.variables.append(self.alloc, std.StringHashMap(Symbol).init(self.alloc));
    for (block.items) |statement|
        try self.compileStatement(statement);

    var last_scope = self.variables.pop().?;
    last_scope.deinit();
}

fn compileVariableDeclaration(self: *Self, statement: ast.Statement) CompilerError!void {
    const var_decl = statement.variable_declaration;

    if (self.findVariable(var_decl.variable_name) != null) {
        utils.print("Variable redeclaration at {f}\n", .{try self.parser.getStatementPos(statement)}, .red);
        return error.VariableRedeclaration;
    }

    const current_block = llvm.core.LLVMGetInsertBlock(self.builder);
    const current_fn = llvm.core.LLVMGetBasicBlockParent(current_block);
    const var_name = try self.cString(var_decl.variable_name);

    switch (var_decl.assigned_value) {
        .struct_instantiation => |inst| {
            const user_type = self.types.get(inst.name) orelse return error.UndeclaredType;
            const struct_type = user_type.llvm_type;

            const alloca = self.createEntryBlockAlloca(current_fn, struct_type, var_name);
            try self.compileStructInit(inst, alloca);
            try self.addVariable(var_decl.variable_name, .{ .local = .{ .ptr = alloca, .ty = struct_type, .is_mut = var_decl.is_mut } });
        },
        else => {
            const value = try self.compileExpression(var_decl.assigned_value);
            const value_type = llvm.core.LLVMTypeOf(value);

            const alloca = self.createEntryBlockAlloca(current_fn, value_type, var_name);
            _ = llvm.core.LLVMBuildStore(self.builder, value, alloca);
            try self.addVariable(var_decl.variable_name, .{ .local = .{ .ptr = alloca, .ty = value_type, .is_mut = var_decl.is_mut } });
        },
    }
}

fn compileReturnStatement(self: *Self, return_expr: ?ast.Expression) CompilerError!void {
    if (return_expr) |re| {
        const compiled_value = try self.compileExpression(re);
        _ = llvm.core.LLVMBuildRet(self.builder, compiled_value);
    } else _ = llvm.core.LLVMBuildRetVoid(self.builder);
}

const LValue = struct {
    ptr: llvm.types.LLVMValueRef,
    ty: llvm.types.LLVMTypeRef,
    is_mut: bool,
};

fn compileLValue(self: *Self, expr: ast.Expression) !LValue {
    return switch (expr) {
        .ident => |ident| switch (self.findVariable(ident) orelse return error.UndeclaredVariable) {
            .local => |l| .{ .ptr = l.ptr, .ty = l.ty, .is_mut = l.is_mut },
            .parameter => |p| .{ .ptr = p.val, .ty = p.ty, .is_mut = p.is_mut },
        },
        .member => |member_access| {
            const lhs_lval = try self.compileLValue(member_access.lhs.*);

            const struct_ptr = lhs_lval.ptr;
            const struct_type: llvm.types.LLVMTypeRef =
                if (member_access.lhs.* == .ident and std.mem.eql(u8, member_access.lhs.ident, "self"))
                    self.current_self_type orelse return error.UnsupportedExpression
                else if (llvm.core.LLVMGetTypeKind(lhs_lval.ty) == .LLVMPointerTypeKind)
                    llvm.core.LLVMGetElementType(lhs_lval.ty)
                else if (llvm.core.LLVMGetTypeKind(lhs_lval.ty) == .LLVMStructTypeKind)
                    return error.UnsupportedExpression
                else
                    return error.UnsupportedExpression;

            if (llvm.core.LLVMGetTypeKind(struct_type) != .LLVMStructTypeKind)
                return error.MemberExpressionOnPrimitiveType;

            const struct_name_z = llvm.core.LLVMGetStructName(struct_type);
            if (struct_name_z[0] == 0) return error.UnsupportedExpression;
            const struct_name = std.mem.span(struct_name_z);

            const user_type = self.types.get(struct_name) orelse return error.UndeclaredType;

            if (member_access.rhs.* != .ident) return error.UnsupportedExpression;
            const field_name = member_access.rhs.ident;

            const field_index = user_type.fields.get(field_name) orelse return error.UndeclaredField;

            const field_ptr = llvm.core.LLVMBuildStructGEP2(self.builder, struct_type, struct_ptr, field_index, "fieldptr");

            const field_type = llvm.core.LLVMStructGetTypeAtIndex(struct_type, field_index);

            return LValue{ .ptr = field_ptr, .ty = field_type, .is_mut = lhs_lval.is_mut };
        },
        else => error.UnsupportedExpression,
    };
}

fn compileMemberAccessExpression(self: *Self, member_access: ast.Expression.Member) !llvm.types.LLVMValueRef {
    const lhs_val = try self.compileExpression(member_access.lhs.*);
    const lhs_type = llvm.core.LLVMTypeOf(lhs_val);

    var struct_ptr: llvm.types.LLVMValueRef = undefined;
    var struct_type: llvm.types.LLVMTypeRef = undefined;

    if (llvm.core.LLVMGetTypeKind(lhs_type) == .LLVMPointerTypeKind) {
        struct_ptr = lhs_val;
        struct_type = llvm.core.LLVMGetElementType(lhs_type);
    } else if (llvm.core.LLVMGetTypeKind(lhs_type) == .LLVMStructTypeKind) {
        const current_fn = llvm.core.LLVMGetBasicBlockParent(llvm.core.LLVMGetInsertBlock(self.builder));
        const temp_alloca = self.createEntryBlockAlloca(current_fn, lhs_type, "temp_struct_val");
        _ = llvm.core.LLVMBuildStore(self.builder, lhs_val, temp_alloca);
        struct_ptr = temp_alloca;
        struct_type = lhs_type;
    } else {
        return error.UnsupportedExpression;
    }

    if (llvm.core.LLVMGetTypeKind(struct_type) != .LLVMStructTypeKind) {
        std.debug.print("compileLValue: LHS is not a struct type after dereferencing. Kind: {s}\n", .{@tagName(llvm.core.LLVMGetTypeKind(struct_type))});
        return error.UnsupportedExpression;
    }
    const struct_name_z = llvm.core.LLVMGetStructName(struct_type);
    if (struct_name_z[0] == 0) {
        return error.UnsupportedExpression;
    }
    const struct_name = std.mem.span(struct_name_z);

    const user_type = self.types.get(struct_name) orelse return error.UndeclaredType;

    if (member_access.rhs.* != .ident) {
        return error.UnsupportedExpression;
    }
    const field_name = member_access.rhs.ident;

    const field_index = user_type.fields.get(field_name) orelse return error.UndeclaredField;

    const field_ptr = llvm.core.LLVMBuildStructGEP2(self.builder, struct_type, struct_ptr, field_index, "fieldptr");
    const field_type = llvm.core.LLVMStructGetTypeAtIndex(struct_type, field_index);
    return llvm.core.LLVMBuildLoad2(self.builder, field_type, field_ptr, "loadtmp");
}

fn compileStructInit(self: *Self, inst: ast.Expression.StructInstantiation, dest: llvm.types.LLVMValueRef) !void {
    const user_type = self.types.get(inst.name) orelse return error.UndeclaredType;
    const struct_type = user_type.llvm_type;

    var it = inst.members.iterator();
    while (it.next()) |entry| {
        const field_name = entry.key_ptr.*;
        const field_expr = entry.value_ptr.*;
        const field_index = user_type.fields.get(field_name) orelse return error.UndeclaredField;
        const value_to_set = try self.compileExpression(field_expr);
        const field_ptr = llvm.core.LLVMBuildStructGEP2(self.builder, struct_type, dest, field_index, "fieldptr");
        _ = llvm.core.LLVMBuildStore(self.builder, value_to_set, field_ptr);
    }
}

fn compileStructInstantiation(self: *Self, inst: ast.Expression.StructInstantiation) !llvm.types.LLVMValueRef {
    const user_type = self.types.get(inst.name) orelse return error.UndeclaredType;
    const struct_type = user_type.llvm_type;
    const alloca = self.createEntryBlockAlloca(llvm.core.LLVMGetBasicBlockParent(llvm.core.LLVMGetInsertBlock(self.builder)), struct_type, "");
    try self.compileStructInit(inst, alloca);
    return llvm.core.LLVMBuildLoad2(self.builder, struct_type, alloca, "loadtmp");
}

fn compileCallExpression(self: *Self, call_expr: ast.Expression.Call) !llvm.types.LLVMValueRef {
    var compiled_args = try std.ArrayList(llvm.types.LLVMValueRef).initCapacity(self.alloc, call_expr.args.items.len);
    defer compiled_args.deinit(self.alloc);

    for (call_expr.args.items) |arg| {
        try compiled_args.append(self.alloc, try self.compileExpression(arg));
    }

    switch (call_expr.callee.*) {
        .member => |member_expr| {
            // Method Call
            const lhs = member_expr.lhs.*;
            if (member_expr.rhs.* != .ident) return error.UnsupportedExpression;
            const method_name = member_expr.rhs.ident;

            // Determine the actual struct type for mangling and lookup.
            var struct_type_for_lookup: llvm.types.LLVMTypeRef = undefined;
            const lhs_val_for_type_check = try self.compileExpression(lhs);
            const lhs_val_type_for_type_check = llvm.core.LLVMTypeOf(lhs_val_for_type_check);

            if (llvm.core.LLVMGetTypeKind(lhs_val_type_for_type_check) == .LLVMPointerTypeKind) {
                struct_type_for_lookup = llvm.core.LLVMGetElementType(lhs_val_type_for_type_check);
            } else if (llvm.core.LLVMGetTypeKind(lhs_val_type_for_type_check) == .LLVMStructTypeKind) {
                struct_type_for_lookup = lhs_val_type_for_type_check;
            } else {
                return error.UnsupportedExpression; // LHS is not a struct or pointer to struct
            }

            const struct_name_z = llvm.core.LLVMGetStructName(struct_type_for_lookup);
            if (struct_name_z[0] == 0) {
                std.debug.print("compileCallExpression: Anonymous struct not supported.\n", .{});
                return error.UnsupportedExpression;
            }
            const struct_name = std.mem.span(struct_name_z);
            const mangled_name: [*:0]const u8 = try std.fmt.allocPrintSentinel(self.alloc, "__MANGLE_{s}_{s}", .{ struct_name, method_name }, 0);
            const func_to_call = llvm.core.LLVMGetNamedFunction(self.module, mangled_name) orelse return error.UndeclaredVariable;
            const fn_type = llvm.core.LLVMGlobalGetValueType(func_to_call); // This is the function type

            var self_arg: llvm.types.LLVMValueRef = undefined;

            // Get the number of parameters
            const param_count = llvm.core.LLVMCountParams(func_to_call);
            if (param_count == 0) return error.UnsupportedExpression; // Or handle no parameters

            // Allocate an array to hold the parameter types
            const param_types_buffer = try self.alloc.alloc(llvm.types.LLVMTypeRef, param_count);
            defer self.alloc.free(param_types_buffer);

            // Get the parameter types
            llvm.core.LLVMGetParamTypes(fn_type, param_types_buffer.ptr);

            const expected_first_param_type = param_types_buffer[0];

            // Compile the LHS expression to get its value.
            const lhs_val = try self.compileExpression(lhs);
            const lhs_val_type = llvm.core.LLVMTypeOf(lhs_val);

            if (llvm.core.LLVMGetTypeKind(expected_first_param_type) == .LLVMPointerTypeKind) {
                // Method expects a pointer (e.g., self: &mut Self)
                if (llvm.core.LLVMGetTypeKind(lhs_val_type) == .LLVMPointerTypeKind) {
                    self_arg = lhs_val; // LHS is already a pointer
                } else if (llvm.core.LLVMGetTypeKind(lhs_val_type) == .LLVMStructTypeKind) {
                    // LHS is a struct value, but method expects a pointer.
                    // Allocate space, store the value, and pass the pointer.
                    const current_fn = llvm.core.LLVMGetBasicBlockParent(llvm.core.LLVMGetInsertBlock(self.builder));
                    const temp_alloca = self.createEntryBlockAlloca(current_fn, lhs_val_type, "self_temp_ptr");
                    _ = llvm.core.LLVMBuildStore(self.builder, lhs_val, temp_alloca);
                    self_arg = temp_alloca;
                } else {
                    return error.UnsupportedExpression; // Type mismatch
                }
            } else if (llvm.core.LLVMGetTypeKind(expected_first_param_type) == .LLVMStructTypeKind) {
                // Method expects a struct by value (e.g., lhs: Self)
                if (llvm.core.LLVMGetTypeKind(lhs_val_type) == .LLVMStructTypeKind) {
                    self_arg = lhs_val; // LHS is already a struct value
                } else if (llvm.core.LLVMGetTypeKind(lhs_val_type) == .LLVMPointerTypeKind) {
                    // LHS is a pointer to a struct, but method expects a struct value.
                    // Load the value from the pointer.
                    self_arg = llvm.core.LLVMBuildLoad2(self.builder, llvm.core.LLVMGetElementType(lhs_val_type), lhs_val, "loadtmp_self_val");
                } else {
                    return error.UnsupportedExpression; // Type mismatch
                }
            } else {
                return error.UnsupportedExpression; // Unsupported first parameter type
            }

            var final_args = try std.ArrayList(llvm.types.LLVMValueRef).initCapacity(self.alloc, compiled_args.items.len + 1);
            defer final_args.deinit(self.alloc);
            try final_args.append(self.alloc, self_arg);
            try final_args.appendSlice(self.alloc, compiled_args.items);

            return llvm.core.LLVMBuildCall2(self.builder, fn_type, func_to_call, final_args.items.ptr, cUint(final_args.items.len), "calltmp");
        },
        .ident => |ident| {
            // Normal function call
            const func_name = try self.cString(ident);
            const func_to_call = llvm.core.LLVMGetNamedFunction(self.module, func_name) orelse return error.UndeclaredVariable;

            const fn_type = llvm.core.LLVMGlobalGetValueType(func_to_call);
            return llvm.core.LLVMBuildCall2(self.builder, fn_type, func_to_call, compiled_args.items.ptr, cUint(compiled_args.items.len), "calltmp");
        },
        else => return error.UnsupportedExpression,
    }
}

fn compileExpression(self: *Self, expr: ast.Expression) CompilerError!llvm.types.LLVMValueRef {
    return switch (expr) {
        .call => |call_expr| return try self.compileCallExpression(call_expr),
        .member => |member_access| return try self.compileMemberAccessExpression(member_access),
        .struct_instantiation => |inst| return try self.compileStructInstantiation(inst),
        .uint => |uint| {
            const i32_type = llvm.core.LLVMInt32TypeInContext(self.context);
            return llvm.core.LLVMConstInt(i32_type, uint, 0);
        },
        .ident => |ident| {
            const symbol = self.findVariable(ident) orelse return error.UndeclaredVariable;
            return switch (symbol) {
                .parameter => |p| p.val,
                .local => |l| {
                    const variable_name = try self.cString(ident);
                    const pointee_type = l.ty;
                    return llvm.core.LLVMBuildLoad2(self.builder, pointee_type, l.ptr, variable_name);
                },
            };
        },
        .assignment => |assignment| {
            const lval = try self.compileLValue(assignment.assignee.*);
            if (!lval.is_mut) return error.AssignmentToImmutableVariable;

            const rhs_value = try self.compileExpression(assignment.value.*);

            // Handle compound assignment operators like +=
            const final_value = switch (assignment.op) {
                .equals => rhs_value,
                .minus_equals => blk: {
                    const current_val = llvm.core.LLVMBuildLoad2(self.builder, lval.ty, lval.ptr, "loadtmp");
                    const new_val = llvm.core.LLVMBuildSub(self.builder, current_val, rhs_value, "subtmp");
                    break :blk new_val;
                },
                // TODO: Add other compound assignment operators
                else => return error.UnsupportedExpression,
            };

            _ = llvm.core.LLVMBuildStore(self.builder, final_value, lval.ptr);
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

                else => {
                    std.debug.print("compileLValue: Unhandled expression type for LValue. Kind: {s}\n", .{@tagName(expr)});
                    return error.UnsupportedExpression;
                },
            };
        },
        else => {
            std.debug.print("Unsupported expression type: {s}\n", .{@tagName(expr)});
            return error.UnsupportedExpression;
        },
    };
}

fn compileForStatement(self: *Self, for_stmt: ast.Statement.For) !void {
    const iterator_expr = for_stmt.iterator.*;
    if (iterator_expr != .range) {
        // TODO: support other iterators
        return error.UnsupportedExpression;
    }
    const range = iterator_expr.range;

    const start_val = try self.compileExpression(range.start.*);
    const end_val = try self.compileExpression(range.end.*);

    const current_fn = llvm.core.LLVMGetBasicBlockParent(llvm.core.LLVMGetInsertBlock(self.builder));

    // Create the loop counter variable
    const i32_type = llvm.core.LLVMInt32TypeInContext(self.context); // Assuming i32 for now
    const counter_alloca = self.createEntryBlockAlloca(current_fn, i32_type, "i");
    _ = llvm.core.LLVMBuildStore(self.builder, start_val, counter_alloca);

    // Create basic blocks
    const cond_block = llvm.core.LLVMAppendBasicBlockInContext(self.context, current_fn, "for_cond");
    const loop_block = llvm.core.LLVMAppendBasicBlockInContext(self.context, current_fn, "for_loop");
    const after_block = llvm.core.LLVMAppendBasicBlockInContext(self.context, current_fn, "after_for");

    // Jump to condition
    _ = llvm.core.LLVMBuildBr(self.builder, cond_block);

    // --- Condition Block ---
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, cond_block);
    const counter_val = llvm.core.LLVMBuildLoad2(self.builder, i32_type, counter_alloca, "i_val");
    const cond = llvm.core.LLVMBuildICmp(self.builder, .LLVMIntSLT, counter_val, end_val, "forcond");
    _ = llvm.core.LLVMBuildCondBr(self.builder, cond, loop_block, after_block);

    // --- Loop Body Block ---
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, loop_block);

    // Create a new scope for the loop body
    try self.variables.append(self.alloc, std.StringHashMap(Symbol).init(self.alloc));
    // Add capture variable to scope
    try self.addVariable(for_stmt.capture, .{ .local = .{ .ptr = counter_alloca, .ty = i32_type, .is_mut = false } });

    // Compile body
    try self.compileStatement(for_stmt.body.*);

    // Pop loop body scope
    var last_scope = self.variables.pop().?;
    last_scope.deinit();

    // Increment counter
    const one = llvm.core.LLVMConstInt(i32_type, 1, 0);
    const next_val = llvm.core.LLVMBuildAdd(self.builder, counter_val, one, "next_i");
    _ = llvm.core.LLVMBuildStore(self.builder, next_val, counter_alloca);

    // Jump back to condition
    _ = llvm.core.LLVMBuildBr(self.builder, cond_block);

    // --- Continue ---
    llvm.core.LLVMPositionBuilderAtEnd(self.builder, after_block);
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
        .self_type => {
            return self.current_self_type orelse error.UnsupportedType;
        },
        .reference => |ref| llvm.core.LLVMPointerType(try self.compileType(ref.inner.*), 0),
        .symbol => |sym| {
            if (std.mem.eql(u8, sym, "void")) return llvm.core.LLVMVoidTypeInContext(self.context);
            if (std.mem.eql(u8, sym, "i8")) return llvm.core.LLVMInt8TypeInContext(self.context);
            if (std.mem.eql(u8, sym, "i16")) return llvm.core.LLVMInt16TypeInContext(self.context);
            if (std.mem.eql(u8, sym, "i32")) return llvm.core.LLVMInt32TypeInContext(self.context);
            if (std.mem.eql(u8, sym, "i64")) return llvm.core.LLVMInt64TypeInContext(self.context);
            if (std.mem.eql(u8, sym, "f16")) return llvm.core.LLVMHalfTypeInContext(self.context);
            if (std.mem.eql(u8, sym, "f32")) return llvm.core.LLVMFloatTypeInContext(self.context);
            if (std.mem.eql(u8, sym, "f64")) return llvm.core.LLVMDoubleTypeInContext(self.context);

            if (self.types.get(sym)) |user_type| {
                return user_type.llvm_type;
            }

            return error.UnsupportedType;
        },
        else => error.UnsupportedType,
    };
}

pub inline fn cString(self: *const Self, s: []const u8) ![*:0]const u8 {
    return try std.mem.concatWithSentinel(self.alloc, u8, &.{s}, 0);
}

inline fn cUint(int: usize) u32 {
    return @as(u32, @intCast(int));
}

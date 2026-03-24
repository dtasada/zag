const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const compiler = @import("compiler.zig");

const Type = compiler.Type;

pub const Error = error{
    ArgumentCount,
    ArrayLengthMustBeInteger,
    BadMemberAccess,
    BadMutability,
    CannotDereference,
    DoubleReturn,
    ExpressionNotCallable,
    IllegalOperator,
    TypeMismatch,
    UnknownSymbol,
} || std.mem.Allocator.Error;

pub fn unknownSymbol(symbol: []const u8, pos: utils.Position) Error {
    return utils.printErr(
        error.UnknownSymbol,
        "Compiler error: Unknown symbol '{s}' ({f}).\n",
        .{ symbol, pos },
    );
}

pub fn arrayLengthMustBeInteger(received: Type, pos: utils.Position) Error {
    return utils.printErr(
        error.ArrayLengthMustBeInteger,
        "Compiler error: array length must be 'usize', received expression of type '{f}' ({f}).\n",
        .{ received, pos },
    );
}

pub fn typeMismatch(expected: Type, received: Type, pos: utils.Position) Error {
    return utils.printErr(
        error.TypeMismatch,
        "Compilation error: Expected '{f}', received '{f}' ({f}).\n",
        .{ expected, received, pos },
    );
}

pub fn typesIncompatible(a: Type, b: Type, pa: utils.Position, pb: utils.Position) Error {
    return utils.printErr(
        error.TypeMismatch,
        "Compilation error: Expressions of type '{f}' ({f}) and '{f}' ({f}) must be compatible.\n",
        .{ a, pa, b, pb },
    );
}

pub fn doubleReturn(pa: utils.Position, pb: utils.Position) Error {
    return utils.printErr(
        error.DoubleReturn,
        "Compilation error: Block returns more than once. First return at {f}, second return at {f}.\n",
        .{ pa, pb },
    );
}

pub fn expressionNotCallable(t: Type, pos: utils.Position) Error {
    return utils.printErr(
        error.ExpressionNotCallable,
        "Compilation error: Expression of type '{f}' is not callable ({f}).\n",
        .{ t, pos },
    );
}

pub fn cannotDereference(t: Type, p: utils.Position) Error {
    return utils.printErr(
        error.CannotDereference,
        "Compilation error: Cannot dereference expression of type '{f}' ({f}).\n",
        .{ t, p },
    );
}

pub fn cannotIndex(t: Type, p: utils.Position) Error {
    return utils.printErr(
        error.CannotDereference,
        "Compilation error: Cannot index expression of type '{f}' ({f}).\n",
        .{ t, p },
    );
}

pub fn cannotSlice(t: Type, p: utils.Position) Error {
    return utils.printErr(
        error.CannotDereference,
        "Compilation error: Cannot slice expression of type '{f}' ({f}).\n",
        .{ t, p },
    );
}

pub fn badMemberAccessSlice(t: Type, received: []const u8, p: utils.Position) Error {
    return utils.printErr(
        error.BadMemberAccess,
        "Compiler error: Attempted to access member '{s}' of '{f}'. Slices only have members 'ptr' and 'len' ({f}).\n",
        .{ received, t, p },
    );
}

pub fn badMemberAccess(parent: Type, received: []const u8, p: utils.Position) Error {
    return utils.printErr(
        error.BadMemberAccess,
        "Compiler error: Cannot access member '{s}' of primitive type '{f}' ({f}).\n",
        .{ received, parent, p },
    );
}

pub fn illegalPrefixOp(rhs: Type, op: ast.PrefixOperator, pos: utils.Position) Error {
    return utils.printErr(
        error.IllegalOperator,
        "Compiler error: Illegal prefix operator '{f}' used on expression of type '{f}' ({f}).\n",
        .{ op, rhs, pos },
    );
}

pub fn mutRefOfConst(pos: utils.Position) Error {
    return utils.printErr(
        error.BadMutability,
        "Compiler error: Attempting to take a mutable reference of non-mutable expression ({f}).\n",
        .{pos},
    );
}

pub fn exprIsNotStruct(received: Type, pos: utils.Position) Error {
    return utils.printErr(
        error.TypeMismatch,
        "Compiler error: Expected compound type, found '{f}' ({f}).\n",
        .{ received, pos },
    );
}

pub fn argumentCount(expected: usize, received: usize, pos: utils.Position) Error {
    return utils.printErr(
        error.ArgumentCount,
        "Compiler error: Expected {} arguments in function call, received {} ({f}).\n",
        .{ expected, received, pos },
    );
}

pub fn unknownMember(parent: Type, name: []const u8, pos: utils.Position) Error {
    return utils.printErr(
        error.UnknownSymbol,
        "Compiler error: '{s}' is not a member of '{f}' ({f}).\n",
        .{ name, parent, pos },
    );
}

pub fn assignmentOnNonMut(pos: utils.Position) Error {
    return utils.printErr(
        error.BadMutability,
        "Compiler error: Assignment on non-mutable expression ({f}).\n",
        .{pos},
    );
}

pub fn derefNonPtr(t: Type, pos: utils.Position) Error {
    return utils.printErr(
        error.TypeMismatch,
        "Compiler error: Attempting to dereference expression of non-reference type '{f}' ({f}).\n",
        .{ t, pos },
    );
}

pub fn illegalIndex(t: Type, pos: utils.Position) Error {
    return utils.printErr(
        error.TypeMismatch,
        "Compiler error: Attempting to index on expression of type '{f}' ({f}).\n",
        .{ t, pos },
    );
}

pub fn illegalIndexType(t: Type, pos: utils.Position) Error {
    return utils.printErr(
        error.TypeMismatch,
        "Compiler error: Index must be an integer, received expression of type '{f}' ({f}).\n",
        .{ t, pos },
    );
}

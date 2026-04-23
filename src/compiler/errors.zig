const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const compiler = @import("compiler.zig");

const Type = compiler.Type;

pub const Error = error{
    ArgumentCount,
    ArrayLengthMustBeInteger,
    EnumMemberMustBeInteger,
    BadMemberAccess,
    BadMutability,
    CannotDereference,
    DoubleReturn,
    ExpressionNotCallable,
    IllegalReturn,
    IllegalOperator,
    MissingMembers,
    TypeMismatch,
    UnknownSymbol,
} || std.mem.Allocator.Error;

pub fn unknownSymbol(io: std.Io, symbol: []const u8, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.UnknownSymbol,
        "Compiler error: Unknown symbol '{s}' ({f}).\n",
        .{ symbol, pos },
    );
}

pub fn arrayLengthMustBeInteger(io: std.Io, received: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.ArrayLengthMustBeInteger,
        "Compiler error: array length must be 'usize', received expression of type '{f}' ({f}).\n",
        .{ received, pos },
    );
}

pub fn enumMemberMustBeInteger(io: std.Io, received: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.EnumMemberMustBeInteger,
        "Compiler error: Enum members must be 'usize', received expression of type '{f}' ({f}).\n",
        .{ received, pos },
    );
}

pub fn typeMismatch(io: std.Io, expected: Type, received: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compilation error: Expected '{f}', received '{f}' ({f}).\n",
        .{ expected, received, pos },
    );
}

pub fn typesIncompatible(io: std.Io, a: Type, b: Type, pa: utils.Position, pb: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compilation error: Expressions of type '{f}' ({f}) and '{f}' ({f}) must be compatible.\n",
        .{ a, pa, b, pb },
    );
}

pub fn doubleReturn(io: std.Io, pa: utils.Position, pb: utils.Position) Error {
    return utils.printErr(
        io,
        error.DoubleReturn,
        "Compilation error: Block returns more than once. First return at {f}, second return at {f}.\n",
        .{ pa, pb },
    );
}

pub fn expressionNotCallable(io: std.Io, t: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.ExpressionNotCallable,
        "Compilation error: Expression of type '{f}' is not callable ({f}).\n",
        .{ t, pos },
    );
}

pub fn cannotDereference(io: std.Io, t: Type, p: utils.Position) Error {
    return utils.printErr(
        io,
        error.CannotDereference,
        "Compilation error: Cannot dereference expression of type '{f}' ({f}).\n",
        .{ t, p },
    );
}

pub fn cannotIndex(io: std.Io, t: Type, p: utils.Position) Error {
    return utils.printErr(
        io,
        error.CannotDereference,
        "Compilation error: Cannot index expression of type '{f}' ({f}).\n",
        .{ t, p },
    );
}

pub fn cannotSlice(io: std.Io, t: Type, p: utils.Position) Error {
    return utils.printErr(
        io,
        error.CannotDereference,
        "Compilation error: Cannot slice expression of type '{f}' ({f}).\n",
        .{ t, p },
    );
}

pub fn badMemberAccessSlice(io: std.Io, t: Type, received: []const u8, p: utils.Position) Error {
    return utils.printErr(
        io,
        error.BadMemberAccess,
        "Compiler error: Attempted to access member '{s}' of '{f}'. Slices only have members 'ptr' and 'len' ({f}).\n",
        .{ received, t, p },
    );
}

pub fn badMemberAccess(io: std.Io, parent: Type, received: []const u8, p: utils.Position) Error {
    return utils.printErr(
        io,
        error.BadMemberAccess,
        "Compiler error: Cannot access member '{s}' of primitive type '{f}' ({f}).\n",
        .{ received, parent, p },
    );
}

pub fn illegalPrefixOp(io: std.Io, rhs: Type, op: ast.PrefixOperator, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.IllegalOperator,
        "Compiler error: Illegal prefix operator '{f}' used on expression of type '{f}' ({f}).\n",
        .{ op, rhs, pos },
    );
}

pub fn mutRefOfConst(io: std.Io, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.BadMutability,
        "Compiler error: Attempting to take a mutable reference of non-mutable expression ({f}).\n",
        .{pos},
    );
}

pub fn exprIsNotStruct(io: std.Io, received: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Expected compound type, found '{f}' ({f}).\n",
        .{ received, pos },
    );
}

pub fn argumentCount(io: std.Io, expected: usize, received: usize, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.ArgumentCount,
        "Compiler error: Expected {} arguments in function call, received {} ({f}).\n",
        .{ expected, received, pos },
    );
}

pub fn unknownMember(io: std.Io, parent: Type, name: []const u8, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.UnknownSymbol,
        "Compiler error: '{s}' is not a member of '{f}' ({f}).\n",
        .{ name, parent, pos },
    );
}

pub fn assignmentOnNonMut(io: std.Io, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.BadMutability,
        "Compiler error: Assignment on non-mutable expression ({f}).\n",
        .{pos},
    );
}

pub fn derefNonPtr(io: std.Io, t: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Attempting to dereference expression of non-reference type '{f}' ({f}).\n",
        .{ t, pos },
    );
}

pub fn illegalIndex(io: std.Io, t: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Attempting to index on expression of type '{f}' ({f}).\n",
        .{ t, pos },
    );
}

pub fn illegalIndexType(io: std.Io, t: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Index must be an integer, received expression of type '{f}' ({f}).\n",
        .{ t, pos },
    );
}

pub fn badBangPrefix(io: std.Io, t: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Unary operator '!' used on expression of type '{f}'. '!' may only be used on boolean expressions ({f}).\n",
        .{ t, pos },
    );
}

pub fn badDashPrefix(io: std.Io, t: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Unary operator '-' used on expression of type '{f}'. '-' may only be used on integers or floats ({f}).\n",
        .{ t, pos },
    );
}

pub fn typeMismatchBinExpr(io: std.Io, lhs: Type, rhs: Type, op: ast.BinaryOperator, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Two sides of binary expression should be of the same type. Received '{f}' {s} '{f}' ({f}).\n",
        .{ lhs, @tagName(op), rhs, pos },
    );
}

pub fn booleanOperatorUsedOnNumerical(io: std.Io, t: Type, op: ast.BinaryOperator, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Binary operator '{[op]s}' may only be used on boolean expressions, but was used as '{[t]f}' {[op]s} '{[t]f}' ({[pos]f}).\n",
        .{ .op = @tagName(op), .t = t, .pos = pos },
    );
}

pub fn numericalOperatorUsedOnBoolean(io: std.Io, t: Type, op: ast.BinaryOperator, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Binary operator '{[op]s}' may only be used on numerical expressions, but was used as '{[t]f}' {[op]s} '{[t]f}' ({[pos]f}).\n",
        .{ .op = @tagName(op), .t = t, .pos = pos },
    );
}

pub fn missingStructMembers(io: std.Io, t: Type, missing: []const []const u8, pos: utils.Position) Error {
    const m = try std.mem.join(std.heap.c_allocator, "', '", missing);
    defer std.heap.c_allocator.free(m);
    return utils.printErr(
        io,
        error.MissingMembers,
        "Compiler error: In instantiation of struct '{f}': missing members '{s}' ({f}).\n",
        .{ t, m, pos },
    );
}

pub fn extraneousStructMembers(io: std.Io, t: Type, missing: []const []const u8, pos: utils.Position) Error {
    const m = try std.mem.join(std.heap.c_allocator, "', '", missing);
    defer std.heap.c_allocator.free(m);
    return utils.printErr(
        io,
        error.MissingMembers,
        "Compiler error: In instantiation of struct '{f}': extraneous members '{s}' ({f}).\n",
        .{ t, m, pos },
    );
}

pub fn unionMemberCount(io: std.Io, t: Type, received: usize, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.MissingMembers,
        "Compiler error: Instantiation of union type '{f}': union instantiation must include exactly one member, but received {} ({f}).\n",
        .{ t, received, pos },
    );
}

pub fn illegalReturn(io: std.Io, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.IllegalReturn,
        "Compiler error: Return statement outside of function ({f}).\n",
        .{pos},
    );
}

pub fn illegalCondition(io: std.Io, received: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Condition must be boolean or optional, received '{f}' ({f}).\n",
        .{ received, pos },
    );
}

pub fn illegalIterator(io: std.Io, received: Type, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Iterator must be a range, array or slice, received '{f}' ({f}).\n",
        .{ received, pos },
    );
}

pub fn illegalIndexCapture(io: std.Io, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Two captures in non-for statement is illegal ({f}).\n",
        .{pos},
    );
}

pub fn illegalCapture(io: std.Io, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Capturing non-optional expression in condition ({f}).\n",
        .{pos},
    );
}

pub fn illegalReferenceOfCapture(io: std.Io, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Cannot capture reference of range in for loop ({f}).\n",
        .{pos},
    );
}

pub fn arrayInstantiationSizeMismatch(io: std.Io, expected: usize, received: usize, pos: utils.Position) Error {
    return utils.printErr(
        io,
        error.TypeMismatch,
        "Compiler error: Size mismatch in array instantiation: expected {} items, received {} ({f}).\n",
        .{ expected, received, pos },
    );
}

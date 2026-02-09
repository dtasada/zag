const std = @import("std");
const utils = @import("utils");

const Type = @import("Type.zig").Type;

pub const CompilerError = error{
    AccessDenied,
    ArgumentCountMismatch,
    AssignmentToImmutableVariable,
    BadMutability,
    CircularTypeDefinition,
    DuplicateMember,
    ExpressionCannotBeEvaluatedAtCompileTime,
    FailedToCreateParser,
    FailedToReadSource,
    FailedToTokenizeSource,
    GenericArgumentCountMismatch,
    GenericInstantiationFailed,
    IllegalExpression,
    IllegalStatement,
    MemberExpressionOnPrimitiveType,
    MemberIsNotAMethod,
    MissingElseClause,
    MissingReturnStatement,
    NoSuchMember,
    OutOfMemory,
    SymbolNotVariable,
    TypeMismatch,
    TypeNotGeneric,
    TypeNotPrimitive,
    UndeclaredField,
    UndeclaredProperty,
    UndeclaredType,
    UndeclaredVariable,
    UnknownSymbol,
    Unseekable,
    UnsupportedExpression,
    UnsupportedType,
    VariableRedeclaration,
} || @import("Parser").ParserError ||
    @import("Lexer").LexerError ||
    std.fs.Dir.MakeError ||
    std.fs.Dir.OpenError ||
    std.fs.Dir.StatFileError ||
    std.fs.File.OpenError ||
    std.Io.Writer.Error;

pub fn typeMismatch(
    expected_type: Type,
    received_type: Type,
    position: utils.Position,
) CompilerError {
    return utils.printErr(
        error.TypeMismatch,
        "comperr: Expected '{f}', received '{f}' ({f}).\n",
        .{ expected_type, received_type, position },
        .red,
    );
}

pub fn expressionNotCallable(t: Type, position: utils.Position) CompilerError {
    return utils.printErr(
        error.IllegalExpression,
        "comperr: Expression of type '{f}' is not callable ({f}).\n",
        .{ t, position },
        .red,
    );
}

pub fn undeclaredProperty(
    t: Type,
    member_name: []const u8,
    position: utils.Position,
) CompilerError {
    return utils.printErr(
        error.UndeclaredProperty,
        "comperr: '{f}' has no member '{s}' ({f}).\n",
        .{ t, member_name, position },
        .red,
    );
}

pub fn argumentCountMismatch(
    expected_args: usize,
    received_args: usize,
    position: utils.Position,
) CompilerError {
    return utils.printErr(
        error.ArgumentCountMismatch,
        "comperr: Expected {} arguments in function call, found {} ({f}).\n",
        .{ expected_args, received_args, position },
        .red,
    );
}

pub fn unknownSymbol(symbol: []const u8, position: utils.Position) CompilerError {
    std.debug.dumpCurrentStackTrace(null);
    return utils.printErr(
        error.UnknownSymbol,
        "comperr: Unknown symbol '{s}' at {f}.\n",
        .{ symbol, position },
        .red,
    );
}

pub fn illegalMemberExpression(lhs: Type, pos: utils.Position) CompilerError {
    return utils.printErr(
        error.IllegalExpression,
        "comperr: Member expression on '{f}' is illegal ({f})\n",
        .{ lhs, pos },
        .red,
    );
}

pub fn illegalCallExpression(lhs: Type, pos: utils.Position) CompilerError {
    return utils.printErr(
        error.IllegalExpression,
        "comperr: Call expression on '{f}' is illegal ({f})\n",
        .{ lhs, pos },
        .red,
    );
}

pub fn illegalSliceExpression(lhs: Type, pos: utils.Position) CompilerError {
    return utils.printErr(
        error.IllegalExpression,
        "comperr: Slice expression on '{f}' is illegal ({f})\n",
        .{ lhs, pos },
        .red,
    );
}

pub fn genericArgumentCountMismatch(expected: usize, received: usize, pos: utils.Position) CompilerError {
    return utils.printErr(
        error.GenericArgumentCountMismatch,
        "comperr: Expected {} generic arguments, got {} ({f}).\n",
        .{ expected, received, pos },
        .red,
    );
}

pub fn badMutability(position: utils.Position) CompilerError {
    return utils.printErr(
        error.BadMutability,
        "comperr: Assignment expression on immutable binding ({f}).\n",
        .{position},
        .red,
    );
}

pub fn symbolShadowing(name: []const u8, pos: utils.Position) CompilerError {
    return utils.printErr(
        error.IllegalStatement,
        "comperr: Symbol shadowing is not allowed: attempt to redeclare '{s}' ({f}).\n",
        .{ name, pos },
        .red,
    );
}

pub fn illegalPrefixExpression(op: @import("Parser").ast.PrefixOperator, t: Type, pos: utils.Position) CompilerError {
    return utils.printErr(
        error.IllegalExpression,
        "comperr: Illegal prefix operator '{s}' used on '{f}' ({f}).\n",
        .{ @tagName(op), t, pos },
        .red,
    );
}

pub fn badAccess(T: utils.CompoundTypeTag, name: []const u8, pos: utils.Position) CompilerError {
    return utils.printErr(
        error.IllegalExpression,
        "comperr: {s} variable '{s}' must be 'pub' to be accessed from outside its module ({f}).\n",
        .{ @tagName(T), name, pos },
        .red,
    );
}

pub fn typeMismatchIfExpression(body_type: Type, else_type: Type, pos: utils.Position) CompilerError {
    return utils.printErr(
        error.TypeMismatch,
        "comperr: Type mismatch in if expression: {f} and {f} are not compatible ({f}).\n",
        .{ body_type, else_type, pos },
        .red,
    );
}

pub fn ifExpressionMustContainElseClause(pos: utils.Position) CompilerError {
    return utils.printErr(
        error.MissingElseClause,
        "comperr: If expression must contain an else clause ({f})\n",
        .{pos},
        .red,
    );
}

pub fn tryExpressionOnNonErrorUnion(t: Type, pos: utils.Position) CompilerError {
    return utils.printErr(
        error.IllegalExpression,
        "comperr: Illegal 'try' expression on type '{f}'. 'try' requires an error union ({f}).\n",
        .{ t, pos },
        .red,
    );
}

pub fn tryExpressionBadReturnType(
    expr: Type,
    function_return: Type,
    pos: utils.Position,
) CompilerError {
    return utils.printErr(
        error.IllegalExpression,
        "comperr: Try expression of type '{f}' not compatible with function of return type '{f}' ({f}).\n",
        .{ expr, function_return, pos },
        .red,
    );
}

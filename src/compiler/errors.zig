const utils = @import("utils");

const CompilerError = @import("Compiler.zig").CompilerError;

const Type = @import("Type.zig").Type;

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

pub fn tooManyArguments(
    expected_args: usize,
    received_args: usize,
    position: utils.Position,
) CompilerError {
    return utils.printErr(
        error.TooManyArguments,
        "comperr: Too many arguments in method call at {f}. Expected {}, found {}.\n",
        .{ position, expected_args, received_args },
        .red,
    );
}

pub fn missingArguments(
    expected_args: usize,
    received_args: usize,
    position: utils.Position,
) CompilerError {
    return utils.printErr(
        error.MissingArguments,
        "comperr: Missing arguments in method call at {f}. Expected {}, found {}.\n",
        .{ position, expected_args, received_args },
        .red,
    );
}

pub fn unknownSymbol(symbol: []const u8, position: utils.Position) CompilerError {
    return utils.printErr(
        error.UnknownSymbol,
        "comperr: Unknown symbol '{s}' at {f}.\n",
        .{ symbol, position },
        .red,
    );
}

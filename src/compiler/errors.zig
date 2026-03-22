const std = @import("std");
const utils = @import("utils");

const compiler = @import("compiler.zig");

const Type = compiler.Type;

pub const Error = error{
    UnknownSymbol,
    ArrayLengthMustBeInteger,
    TypeMismatch,
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

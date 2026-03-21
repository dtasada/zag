const utils = @import("utils");
const compiler = @import("compiler");

const Type = compiler.Type;

const Error = error{
    UnknownSymbol,
    ArrayLengthMustBeInteger,
};

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

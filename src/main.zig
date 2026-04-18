const std = @import("std");
const builtin = @import("builtin");

const utils = @import("utils");
const Lexer = @import("Lexer");
const Parser = @import("Parser");
const Compiler = @import("Compiler");

const build = @import("build.zig");

const help =
    \\Usage: zag [command]
    \\Commands:
    \\  build
    \\  run
    \\
;

const Command = enum {
    build,
    run,
};

/// program entry point. sets up the cli app.
pub fn main(init: std.process.Init) !void {
    const alloc = init.gpa;
    const io = init.io;

    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 2) return utils.printErr(io, error.MissingCommand, help, .{});
    const command = std.meta.stringToEnum(Command, args[1]) orelse
        return utils.printErr(io, error.UnknownCommand, help, .{});
    switch (command) {
        .build => try build.build(alloc, io),
        .run => try build.run(alloc, io),
    }
}

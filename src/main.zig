const std = @import("std");
const clap = @import("clap");
const pretty = @import("pretty");
const builtin = @import("builtin");

const utils = @import("utils");
const Lexer = @import("Lexer");
const Parser = @import("Parser");
const Compiler = @import("Compiler");

const build = @import("build.zig");

const BuildError = error{
    FailedToReadSource,
    FailedToTokenizeSource,
    FailedToCreateParser,
    FailedToParseSource,
    FailedToCreateCompiler,
    FailedToCompileTarget,
    FailedToBuildProject,
    CompilationError,
    InvalidArgument,
};

const Args = enum {
    build,
    run,
};

/// program entry point. sets up the cli app.
pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help        Display this message and exit.
        \\<str>...
    );

    var diag: clap.Diagnostic = .{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = alloc,
    }) catch |err| {
        try diag.reportToFile(.stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0)
        return clap.helpToFile(.stdout(), clap.Help, &params, .{});
    for (res.positionals[0]) |pos| {
        const option = std.meta.stringToEnum(Args, pos) orelse return utils.printErr(
            error.InvalidArgument,
            "Error: Invalid argument {s}\n.",
            .{pos},
            .red,
        );

        switch (option) {
            .build => build.build(alloc) catch |err| return utils.printErr(
                error.FailedToBuildProject,
                "Failed to build project: {}\n",
                .{err},
                .red,
            ),
            .run => try build.run(alloc),
        }
    }
}

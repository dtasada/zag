const std = @import("std");
const cli = @import("cli");
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
pub fn main() void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    var r = try cli.AppRunner.init(alloc);
    const app: cli.App = .{
        .author = "Dani Tasada",
        .version = "0.1.0",
        .command = .{
            .name = "zag",
            .target = .{
                .subcommands = &.{
                    .{
                        .name = "build",
                        .description = .{ .one_line = "Build project" },
                        .target = .{ .action = .{ .exec = build.build } },
                    },
                    .{
                        .name = "run",
                        .description = .{ .one_line = "Build and run the project" },
                        .target = .{ .action = .{ .exec = build.run } },
                    },
                },
            },
        },
    };

    return r.run(&app) catch |err| utils.print(
        "Error: {}",
        .{err},
        .red,
    );
}

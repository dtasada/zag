const std = @import("std");

const Modules = struct {
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,

    const Module = struct {
        name: []const u8,
        mod: *std.Build.Module,

        fn addImport(lhs: *const Module, rhs: *const Module) void {
            lhs.mod.addImport(rhs.name, rhs.mod);
        }
    };

    fn init(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) Modules {
        return .{
            .b = b,
            .target = target,
            .optimize = optimize,
        };
    }

    fn create(self: Modules, name: []const u8, source_file: []const u8) Module {
        return .{
            .name = name,
            .mod = self.b.createModule(.{
                .target = self.target,
                .optimize = self.optimize,
                .root_source_file = self.b.path(source_file),
            }),
        };
    }
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const stdlib_path = b.getInstallPath(.lib, "");
    const options = b.addOptions();
    options.addOption([]const u8, "stdlib_path", stdlib_path);
    const options_mod = options.createModule();

    const modules = Modules.init(b, target, optimize);
    const zag_mod = modules.create("zag", "src/main.zig");
    const exe = b.addExecutable(.{
        .name = "zag",
        .root_module = zag_mod.mod,
    });

    const utils_mod = modules.create("utils", "src/utils.zig");
    const lexer_mod = modules.create("lexer", "src/lexer.zig");
    const parser_mod = modules.create("parser", "src/parser/parser.zig");
    const ast_mod = modules.create("ast", "src/ast/ast.zig");
    const compiler_mod = modules.create("compiler", "src/compiler/compiler.zig");
    zag_mod.mod.addImport("build_options", options_mod);
    compiler_mod.mod.addImport("build_options", options_mod);

    zag_mod.addImport(&lexer_mod);
    zag_mod.addImport(&parser_mod);
    zag_mod.addImport(&compiler_mod);
    zag_mod.addImport(&utils_mod);

    parser_mod.addImport(&utils_mod);
    parser_mod.addImport(&ast_mod);
    lexer_mod.addImport(&utils_mod);
    compiler_mod.addImport(&utils_mod);
    compiler_mod.addImport(&lexer_mod);
    compiler_mod.addImport(&ast_mod);
    parser_mod.addImport(&lexer_mod);
    ast_mod.addImport(&utils_mod);
    ast_mod.addImport(&lexer_mod);

    compiler_mod.addImport(&parser_mod);

    const install_bin = b.step("install-bin", "Install the zag compiler to the system bin directory");
    const install_exe = b.addInstallArtifact(exe, .{});
    install_bin.dependOn(&install_exe.step);

    const install_stdlib = b.step("install-stdlib", "Copy the Zag standard library to the system directory");
    const copy = b.addInstallDirectory(.{
        .source_dir = b.path("lib"), // stdlib .zag files live in lib/ in the repo
        .install_dir = .lib,
        .install_subdir = "",
    });
    install_stdlib.dependOn(&copy.step);

    const install_all = b.getInstallStep();
    install_all.dependOn(install_bin);
    install_all.dependOn(install_stdlib);

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(install_all);

    if (b.args) |args| run_cmd.addArgs(args);

    const check = b.step("check", "Check if zag compiles");
    check.dependOn(&exe.step);
}

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "llvm_wrapper",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const clap = b.dependency("clap", .{});
    const clap_mod = clap.module("clap");
    exe.root_module.addImport("clap", clap_mod);

    const pretty = b.dependency("pretty", .{});
    const pretty_mod = pretty.module("pretty");
    exe.root_module.addImport("pretty", pretty_mod);

    const llvm_dep = b.dependency("llvm", .{
        .target = target,
        .optimize = optimize,
    });
    const llvm_mod = llvm_dep.module("llvm");
    exe.root_module.addImport("llvm", llvm_mod);

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const exe_check = b.addExecutable(.{
        .name = "llvm_wrapper",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    exe_check.root_module.addImport("clap", clap_mod);
    exe_check.root_module.addImport("pretty", pretty_mod);
    exe_check.root_module.addImport("llvm", llvm_mod);

    const check = b.step("check", "Check if llvm_wrapper compiles");
    check.dependOn(&exe_check.step);
}

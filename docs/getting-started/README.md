# 🚀 Getting Started with Zag

Welcome to Zag! This guide will walk you through setting up the Zag compiler and building your very first program.

## 📋 Prerequisites

Before we begin, ensure you have the following installed on your system:

1.  **Zig (0.15.0 or higher)**: Zag is written in Zig and uses its build system.
    *   [Download Zig](https://ziglang.org/download/)
2.  **C Compiler**: Zag transpiles to C, so you need a C compiler like `gcc`, `clang`, or `cc` available in your `$PATH`.

---

## 🛠️ Step 1: Clone the Repository

First, download the Zag source code from GitHub:

```bash
git clone https://github.com/dtasada/zag.git
cd zag
```

## 🏗️ Step 2: Build the Compiler

Zag uses the standard Zig build system. You can build the compiler executable with a single command:

```bash
zig build
```

This will create the `zag` executable in the `./zig-out/bin/` directory.

## 📝 Step 3: Your First Zag Program

Create a new file at `src/main.zag` in the root directory:

```rust
// hello.zag
bind fn printf(fmt: &c_char, args...) c_int;

fn main() i32 {
    printf("Hello, Zag world!\n");
    return 0;
}
```

## 🚀 Step 4: Compile and Run

```bash
# Run the build command
zag build
```

### What happened?
2.  **Transpilation**: The Zag compiler translates the Zag code into C code.
3.  **Native Compilation**: Zag invokes your system's C compiler to produce a native binary.
4.  **Binary**: You can find your final executable in `.zag-out/bin/main`.

Try running it:
```bash
./.zag-out/bin/main
```

---

## 🔍 Troubleshooting

*   **"cc not found"**: Ensure you have a C compiler installed. On macOS, run `xcode-select --install`. On Ubuntu, run `sudo apt install build-essential`.
*   **Zig version mismatch**: Ensure `zig version` reports `0.15.0` or higher.

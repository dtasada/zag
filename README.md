<div align="center">

# ⚡ Zag

**A robust, type-safe systems programming language transpiled to C.**

[![Built with Zig](https://img.shields.io/badge/Built_with-Zig-orange.svg?style=flat-square&logo=zig)](https://ziglang.org/) [![License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat-square)](LICENSE) 

[Features](#features) • [Installation](#installation) • [Usage](#usage) • [Roadmap](#roadmap)

</div>

---

## 📖 About

**Zag** is an experimental statically-typed programming language written in **Zig**. It combines modern language features like optionals, error unions, and type inference with the portability of C.

The compiler works by transpiling Zag source code into C11, which is then compiled by your system's C compiler (e.g., `gcc`, `clang`, or `cc`). This ensures that Zag binaries are highly optimized and portable to any platform that supports C.

## ✨ Features

*   **🚀 Zero Runtime Overhead**: Transpiles to raw C code.
*   **🛡️ Type Safety**: Strong static typing with type inference.
*   **🧩 Modern Constructs**:
    *   **Optionals** (`?T`): Null safety built-in.
    *   **Error Unions** (`!T`): Ergonomic error handling without exceptions.
    *   **Structs & Enums**: Data organization made simple.
*   **🔌 C Interop**: Native binding support via `bind fn`.
*   **🌳 Tree-sitter Grammar**: First-class support for syntax highlighting and tooling.

## 🛠️ Installation

### Prerequisites

*   **Zig**: To build the project yourself, you need Zig 0.15.x [Zig compiler](https://ziglang.org/download/).
*   **C Compiler**: A standard C compiler (`cc`, `gcc`, or `clang`) must be available in your system path.

### Building from Source

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/dtasada/zag.git
    cd zag
    ```

2.  **Build the project:**
    ```bash
    zig build install --prefix ~/.local
    ```
    This will copy the zag compiler into `~/.local/bin/zag`. If `~/.local/bin/` is in your `$PATH`, running `zag` will work. If not, add `~/.local/bin` to your `$PATH`.
    You could also choose any other install prefix.

3.  **Run the compiler:**
    ```bash
    zag build
    ```

## 💻 Usage

Zag uses a syntax familiar to users of Zig, Rust, or Swift.

### Example: Vector Math

```rust
// Define a struct with methods
struct Vector2 {
    x: i32,
    y: i32,

    // Static constructor method
    fn new(x: i32, y: i32) Vector2 {
        return Vector2 {
            x: x,
            y: y,
        };
    }

    // Instance method
    fn add(lhs: Vector2, rhs: Vector2) Vector2 {
        return Vector2 {
            x: lhs.x + rhs.x,
            y: lhs.y + rhs.y,
        };
    }
}

fn main() i32 {
    // Type inference in action
    let vec = Vector2.new(1, 2);
    let lhs = Vector2 {
        x: 10,
        y: 20
    };
    
    // Method call syntax
    let result = lhs.add(vec);
    
    return result.x;
}
```

### Binding to C Functions

Zag makes it easy to bind to existing C libraries:

```rust
// Bind to the standard C printf function
bind fn printf(fmt: &c_char, args...) c_int;

fn main() i32 {
    printf("Hello from %s!\n", "Zag");
    return 0;
}
```

## 🗺️ Roadmap

The project is currently in the **alpha** stage. The following features are planned or in progress:

- [ ] **Configurable Paths**: Remove hardcoded compiler/input paths (CLI Argument parsing).
- [ ] **String Literals**: Support for escape sequences (`\n`, `\t`, `\"`).
- [ ] **Multi-line Comments**: Support for `/* ... */` blocks.
- [ ] **Safety**: Improved integer overflow checks and safe casting.
- [ ] **Comptime**: Expanded compile-time expression evaluation.

## 📄 License

Distributed under the MIT License. See `LICENSE` for more information.

---

<div align="center">
    <sub>Built with ❤️ by <a href="https://github.com/dtasada">Dani Tasada</a></sub>
</div>

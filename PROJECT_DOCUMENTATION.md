# Zag Project Documentation

This document provides a detailed explanation of the `zag` project, a compiler for a custom language with the `.zag` file extension. The compiler is written in Zig and transpiles Zag code to C.

## Project Structure

The project is organized into the following main components:

-   **`build.zig`**: The build script for the project.
-   **`src/main.zig`**: The entry point.
-   **`src/Lexer.zig`**: The lexical analyzer.
-   **`src/parser/`**: The parser and AST definitions.
-   **`src/compiler/`**: The transpiler to C.

## Program Flow

1.  **Lexical Analysis**: Source code -> Tokens.
2.  **Parsing**: Tokens -> Abstract Syntax Tree (AST).
3.  **C Transpilation**: AST -> C code.
4.  **C Compilation**: C code -> Native executable (via system `cc`).

### 1. Entry Point (`src/main.zig`)

-   Parses command-line arguments (currently supports `build`).
-   Orchestrates the pipeline: reads `src/main.zag` (hardcoded), lexes, parses, and compiles it.

### 2. Lexical Analysis (`src/Lexer.zig`)

-   **`Token`**: Defines language tokens (identifiers, keywords, operators).
-   **`tokenize()`**: Iterates source char-by-char.
-   **Source Map**: Maintains a mapping of tokens to line/column positions for error reporting.

### 3. Parsing (`src/parser/`)

-   **Pratt Parser**: Uses a Pratt parser for efficient expression precedence handling.
-   **`src/parser/ast.zig`**: Defines the AST structure (`Statement`, `Expression`, `Type`). AST nodes contain their source position directly (`pos` field).
-   **`src/parser/Parser.zig`**: Core parser logic. Uses lookup tables (`nud`, `led`) for expression parsing and statements.
-   **`src/parser/TypeParser.zig`**: A specialized parser for type annotations (e.g., `[10]i32`, `?u8`).

### 4. C Transpilation (`src/compiler/Compiler.zig`)

-   **`emit()`**: Traverses the AST and writes C code to `.zag-out/`.
-   **Scopes**: Manages symbol tables via a stack of scopes to handle variable shadowing.
-   **Types**:
    -   Primitive types map directly to C types (via typedefs).
    -   **Structs/Unions**: compiled to C structs/unions.
    -   **Optionals**: compiled to C structs with a boolean flag (`is_some`) and payload.
    -   **Error Unions**: compiled to C structs with `is_success` and a union of success/error types.
-   **`zag.h`**: A helper header file (embedded in `src/compiler/zag.h.zig`) is written to `.zag-out/zag/zag.h`. It contains macro definitions for optionals, error unions, and array lists.
-   **C Compiler**: Spawns a child process (default `/usr/bin/cc`) to compile the generated C code.

## Tree-sitter Grammar (`tree-sitter-zag/`)

The project includes a tree-sitter grammar for syntax highlighting and tooling support, mirroring the parser's logic.
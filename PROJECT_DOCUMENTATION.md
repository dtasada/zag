# LLVM-Wrapper Project Documentation

This document provides a detailed explanation of the `llvm-wrapper` project, a compiler for a custom language with the `.zag` file extension. The compiler is written in Zig and uses LLVM for code generation.

## Project Structure

The project is organized into the following main components:

- **`build.zig`**: The build script for the project, which manages dependencies and defines build steps.
- **`src/main.zig`**: The entry point of the compiler.
- **`src/Lexer.zig`**: The lexical analyzer, responsible for tokenizing the source code.
- **`src/parser/`**: The parser, which builds an Abstract Syntax Tree (AST) from the tokens.
- **`src/Compiler.zig`**: The compiler, which generates LLVM IR from the AST.

## Program Flow

The compilation process follows a classic pipeline:

1.  **Lexical Analysis**: The source code is read and converted into a sequence of tokens.
2.  **Parsing**: The tokens are parsed to build an Abstract Syntax Tree (AST).
3.  **Code Generation**: The AST is traversed to generate LLVM Intermediate Representation (IR).

### 1. Entry Point (`src/main.zig`)

The program execution starts in `src/main.zig`. The `main` function is responsible for:

-   Parsing command-line arguments using the `clap` library. The primary command is `build`.
-   Calling the `build` function to start the compilation process.

The `build` function in `src/main.zig` orchestrates the entire compilation pipeline:

1.  It initializes the `Lexer` with the source file.
2.  It tokenizes the source file using the `Lexer.tokenize()` method.
3.  It initializes the `Parser` with the generated tokens.
4.  It parses the tokens into an AST using the `Parser.parse()` method.
5.  It initializes the `Compiler` with the AST.
6.  It compiles the AST into LLVM IR using the `Compiler.emit()` method.
7.  The generated LLVM IR is then written to an object file in the `.zag-out` directory.

### 2. Lexical Analysis (`src/Lexer.zig`)

The `Lexer` is responsible for converting the raw source code into a stream of tokens.

-   **`Token`**: The `Token` enum in `src/Lexer.zig` defines all possible tokens in the language, such as identifiers, keywords, operators, and literals.
-   **`Lexer.tokenize()`**: This is the core function of the lexer. It iterates through the source code character by character and groups them into tokens. It also maintains a source map to track the position of each token, which is crucial for error reporting.

### 3. Parsing (`src/parser/`)

The parser takes the stream of tokens from the `Lexer` and builds an Abstract Syntax Tree (AST). This project uses a Pratt parser, which is known for its elegance and efficiency in handling expression precedence.

-   **`src/parser/ast.zig`**: This file defines the structure of the AST. The `Statement`, `Expression`, and `Type` unions are the fundamental building blocks that represent the program's structure. The AST represents the grammatical structure of the source code.

-   **`src/parser/Parser.zig`**: This is the core of the parser.
    -   It uses two main functions for parsing expressions: `nud` (null denotation) for tokens that appear at the beginning of an expression (e.g., literals, identifiers) and `led` (left denotation) for tokens that appear in the middle of an expression (e.g., binary operators).
    -   It uses lookup tables for statement and expression handlers, which makes the parser easily extensible.
    -   The `parseExpression` function is the heart of the Pratt parser, correctly handling operator precedence and associativity.

-   **`src/parser/statement_handlers.zig` & `src/parser/expression_handlers.zig`**: These files contain the logic for parsing specific language constructs. For example, `statement_handlers.zig` has functions for parsing `let` statements, `if` statements, and `while` loops. `expression_handlers.zig` handles parsing of literals, variables, function calls, etc.

-   **`src/parser/TypeParser.zig`**: This is a specialized sub-parser for handling type annotations. It cleverly reuses the same Pratt parsing design to parse complex type expressions.

### 4. Code Generation (`src/Compiler.zig`)

The `Compiler` is the final stage of the compilation pipeline. It traverses the AST produced by the parser and generates LLVM Intermediate Representation (IR).

-   **`Compiler.emit()`**: This is the main entry point for the compiler. It iterates through the statements in the AST and calls `compileStatement` for each one.

-   **`Compiler.compileStatement()` & `Compiler.compileExpression()`**: These are recursive functions that walk the AST.
    -   `compileStatement` handles statements like `let`, `if`, `while`, and `return`.
    -   `compileExpression` handles expressions like literals, binary operations, function calls, and variable access.

-   **Symbol Table**: The compiler maintains a symbol table to keep track of variables, functions, and their corresponding LLVM values. It uses a stack of scopes to handle variable shadowing and lifetimes.

-   **Type Table**: A type table is used to manage user-defined types, such as structs.

-   **LLVM IR Generation**: The compiler translates AST nodes into LLVM instructions. For example:
    -   A `let` statement is translated into an `alloca` instruction to allocate space on the stack and a `store` instruction to store the value.
    -   An `if` statement is translated into basic blocks and conditional branch instructions.
    -   A `+` operator is translated into an `LLVMBuildAdd` instruction.

-   **Method Calls**: Method calls are implemented by mangling the function name (e.g., `__MANGLE_StructName_methodName`) and passing the struct instance as the first argument to the function.

-   **Object File Emission**: Finally, the compiler uses the LLVM TargetMachine to emit a final object file (`.zag-out/main.o`).

## Build Process (`build.zig`)

The `build.zig` file defines how the project is built. It uses the Zig build system to:

-   Declare the executable and its entry point.
-   Manage dependencies, which include:
    -   `clap`: For command-line argument parsing.
    -   `pretty`: For debugging and pretty-printing.
    -   `llvm`: For the compiler backend.
-   Set up the necessary build and run steps.

This provides a clear overview of the project's external dependencies and how they are integrated.

## Tree-sitter Grammar (`tree-sitter-zag/`)

The project also includes a tree-sitter grammar for the `.zag` language, located in the `tree-sitter-zag/` directory. This grammar is used for syntax highlighting, code navigation, and other tooling features.

The grammar is defined in `tree-sitter-zag/grammar.js`. It is written in JavaScript and uses the tree-sitter DSL to define the language's syntax. The grammar is based on the parser in `src/parser/`, and it is designed to be as close as possible to the parser's behavior.

The grammar is organized into a set of rules that define the language's syntax. The main rule is `source_file`, which is a sequence of statements. The grammar defines rules for all the language's constructs, including:

-   Statements: `let`, `if`, `while`, `for`, `return`, etc.
-   Expressions: literals, binary operations, function calls, etc.
-   Types: `int`, `float`, `bool`, structs, enums, etc.

The grammar also includes rules for comments and other "extra" tokens that can appear anywhere in the source code.

The grammar is used to generate a parser that can be used by various tools to understand and process `.zag` source code.

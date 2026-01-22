# Zag Project Code Audit

This document contains a comprehensive audit of the Zag project, identifying issues ranging from critical safety bugs to performance bottlenecks and architectural flaws.

## 🚨 Critical Bugs & Safety Issues

### 1. Unsafe Implicit Integer Conversions
**File:** `src/compiler/Type.zig`
**Function:** `convertsTo`
**Severity:** Critical
**Description:** The compiler allows implicit conversion between **all** integer types, regardless of sign or size. A `u64` can be assigned to a `u8`, or an `i64` to a `u64`, without any warning or error.
**Snippet:**
```zig
.i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => switch (src) {
    .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => true, // <--- ALLOWS EVERYTHING
    else => false,
},
```
**Impact:** Silent data loss and integer overflows/underflows in compiled code.

### 4. Naive String Parsing (No Escape Sequences)
**File:** `src/Lexer.zig`
**Function:** `tokenize`
**Severity:** Medium
**Description:** The lexer parses strings by simply scanning for the next `"`. It does not handle escaped quotes (`\"`) or any other escape sequences (`\n`, `\t`).
**Snippet:**
```zig
if (std.mem.indexOfScalar(u8, self.input[self.pos..], '"')) |string_end| { ... }
```
**Impact:** Users cannot write strings containing double quotes.

### 5. Compiler Panics on Unimplemented Comptime Expressions
**File:** `src/compiler/Compiler.zig`
**Function:** `solveComptimeExpression`
**Severity:** Medium
**Description:** The function panics if it encounters an expression type it can't handle (like an identifier), instead of returning a proper error.
**Impact:** Valid code like `const N = 10; let a: [N]u8;` will crash the compiler because it can't resolve `N` at comptime (it expects a literal).

## 🏗 Logic & Architecture Flaws

### 1. Bizarre Comment Handling
**File:** `src/Lexer.zig`
**Function:** `parseBinaryOperator`
**Description:** Single-line comments (`//`) are handled *inside* the `parseBinaryOperator` function. This is structurally incorrect.
**Snippet:**
```zig
// Inside parseBinaryOperator...
if (self.pos + 2 <= self.input.len and std.mem.eql(u8, self.input[self.pos .. self.pos + 2], "//")) { ... }
```
**Why it's bad:** Comments should be treated as whitespace/extras at the top level of the tokenizer loop, not as a special case of operator parsing (triggered by `/`).

### 2. Hardcoded Source Limits
**File:** `src/compiler/Compiler.zig`
**Function:** `getAST`
**Description:** The source file reading is hardcoded to a maximum of 1MB (`1 << 20`).
**Snippet:**
```zig
const file = std.fs.cwd().readFileAlloc(alloc, file_path, 1 << 20) catch |err|
```
**Impact:** The compiler will fail on source files larger than 1MB.

### 4. Limited Parser/Lexer State Management
**File:** `src/Lexer.zig`
**Description:** Line and column tracking (`advanceN`) is somewhat manual and brittle.

## 🚀 Performance Bottlenecks

### 1. Heavy Type Hashing/Equality
**File:** `src/compiler/Type.zig`
**Function:** `Context.hash`, `Context.eql`
**Description:** These functions allocate new `std.ArrayList`s for visited nodes on *every* call to handle recursive types.
**Snippet:**
```zig
var buf: [128]Context.Visited = undefined;
var list: std.ArrayList(Context.Visited) = .initBuffer(&buf);
// ... implies overhead for every type comparison
```
**Impact:** Type checking involves massive amounts of small allocations/deallocations (or at least list initializations), slowing down compilation significantly for large projects.

## 🔍 Other Observations

### 1. Hardcoded Paths in Build System
**File:** `build.zig`
**Description:** The build script hardcodes `src/main.zig` as the root source file.

## ✨ Missing Language Features

The following standard features are currently absent from the Zag language and its compiler:

### 1. Control Flow
-   **Loop Control (`break` / `continue`)**: Keywords exist in some contexts but are not implemented in the parser or compiler.
-   **Match Exhaustiveness**: The compiler does not verify that all possible cases (enum variants or union tags) are handled in a `match` expression.
-   **Match Catch-all**: No support for a default/wildcard branch (`else` or `_`) in `match`.
-   **Defer**: No mechanism to schedule cleanup code at the end of a scope.

### 2. Type System & Safety
-   **Explicit Casting (`as`)**: The keyword is recognized by the lexer but has no implementation in the parser or compiler.
-   **Error Handling (`try` / `catch`)**: No syntactic sugar for handling or propagating error unions.
-   **Traits / Interfaces**: No support for defining shared behavior across types (runtime or compile-time polymorphism).
-   **Tuples**: No support for anonymous grouped types.

### 3. Memory & Data Structures
-   **Slices**: No "view" type for arrays, necessitating manual pointer management or expensive copies.
-   **Closures / Lambdas**: Function types are limited to top-level or static function pointers; no environment capture.

### 4. Syntax & UX
-   **String Interpolation**: No support for embedding expressions in strings.
-   **Multi-line Strings**: String literals are restricted to a single line.
-   **Refined Method Visibility**: Cross-module visibility checks for methods are largely unimplemented.

## 👯 Code Duplication & Redundancy

### 1. Function Parsing & Compilation
-   **Locations**: `src/parser/statements.zig` and `src/compiler/statements.zig`.
-   **Issue**: `functionDefinition` and `bindingFunctionDeclaration` share 90% of their logic (parsing name, generics, parameters, return type). They only differ in the function body (block vs. semicolon).
-   **Recommendation**: Extract a common `FunctionSignature` struct and helper methods (`parseFunctionSignature`, `compileFunctionSignature`) to handle the shared parts.

### 2. Dual Declaration Logic (Analysis vs. Emit)
-   **Locations**: `src/compiler/Compiler.zig` (analyze) vs `src/compiler/statements.zig` (emit) vs `src/compiler/Type.zig`.
-   **Issue**: The logic for processing `struct`, `union`, and `enum` declarations is tripled.
    1.  `Compiler.analyze` parses declarations to populate the symbol table.
    2.  `Type.fromCompoundTypeDeclaration` parses declarations to build the internal `Type` representation.
    3.  `statements.compoundTypeDeclaration` parses declarations to emit C typedefs.
-   **Risk**: If you add a new feature to structs (e.g., alignment), you must update it in **three** places. If they desync, the compiler will see one thing but emit another.

### 3. Redundant Pratt Parsers
-   **Locations**: `src/parser/Parser.zig` and `src/parser/TypeParser.zig`.
-   **Issue**: Both files implement their own Pratt parser engine (NUD/LED/BindingPower lookup tables). `TypeParser` is essentially a copy-paste of the expression parser but for types.
-   **Recommendation**: Abstract the core Pratt parsing loop and handler management into a generic `PrattParser(T)` struct.

### 4. Repeated Error Handling Patterns
-   **Locations**: Throughout the compiler.
-   **Issue**: Manual calls to `utils.printErr` with format strings are scattered everywhere.
-   **Recommendation**: Centralize error reporting in `src/compiler/errors.zig` for all error types, ensuring consistent formatting and easier refactoring.

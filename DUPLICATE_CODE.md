# Duplicate Code Report

This document outlines areas of duplicated or semantically similar code in the Zag compiler codebase that could be refactored for better maintainability and abstraction.

## 1. `src/compiler/Type.zig`: Builtin Generic Function Instantiation

In the `instantiateGeneric` function, the logic for handling the builtin generic functions `sizeof`, `cast`, and `xor` is highly repetitive.

### 1.1. Identical Code for `cast` and `xor`

The code blocks for handling the `cast` and `xor` functions are identical. This is a clear case of copy-pasted code.

**Location:** `src/compiler/Type.zig`, `instantiateGeneric` function.

**Description:** The `else if` blocks for `base_type.function.name == "cast"` and `base_type.function.name == "xor"` are the same.

### 1.2. Similar Structure for `sizeof`, `cast`, and `xor`

All three builtin functions (`sizeof`, `cast`, and `xor`) follow a similar instantiation pattern:

1.  A new function type is created by copying the base function type.
2.  The name, generic parameters, and generic instantiation info are updated.
3.  The new function type is registered globally.
4.  The new function type is returned.

This shared logic could be extracted into a helper function to reduce repetition and improve clarity.

## 2. `src/compiler/Type.zig`: Compound Type Initialization

In the `fromCompoundTypeDeclaration` function, the logic for initializing a new compound type (`struct`, `union`, or `enum`) is duplicated.

**Location:** `src/compiler/Type.zig`, `fromCompoundTypeDeclaration` function.

**Description:** The `else` branch of the `if (compiler.getSymbolType(type_decl.name))` condition contains a call to `.init` that is nearly identical to the fallback `break :b try .init(...)` call inside the `if` branch. This initialization logic can be extracted to avoid duplication.

## 3. `src/compiler/statements.zig`: Function and Method Compilation

There is significant overlap between the logic for compiling standalone functions (`functionDefinition` function) and the logic for compiling methods within a compound type (`compoundTypeDeclaration` function).

**Location:** `src/compiler/statements.zig`

**Description:** Both places have similar code for:
-   Generating the function/method header declaration.
-   Generating the function/method implementation signature.
-   Handling return types.
-   Registering parameters in a new scope.
-   Calling `compileBlock` to compile the function/method body.

This duplicated logic could be refactored into a more generic function for compiling any function-like construct.

## 4. `src/compiler/Compiler.zig`: AST Traversal in `scan` and `analyze`

The `scan` and `analyze` functions in `src/compiler/Compiler.zig` both traverse the AST to perform symbol registration and analysis. There is a lot of duplicated logic and repeated traversal of the AST.

**Location:** `src/compiler/Compiler.zig`

**Description:**
-   The `scan` function iterates over the AST twice.
-   The `analyze` function calls `scan` and then iterates over the AST again.
-   The logic for processing `function_definition` is very similar in `scan` (second loop) and `analyze`.

This repeated traversal and duplicated logic could be consolidated into a single, more efficient pass over the AST, or the shared logic could be extracted into helper functions that are called from both `scan` and `analyze`. This would make the code easier to understand and maintain.

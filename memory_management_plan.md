# Manual Memory Management Plan

This document outlines the necessary `deinit` methods to be implemented for the Zag compiler to transition from an arena allocator to manual memory management.

The analysis has revealed that the `ArenaAllocator` in `src/build.zig` is masking numerous memory leaks across the parser and compiler. The following data structures are allocated on the heap but are not properly deinitialized.

## I. Lexer and Parser (`src/`)

The primary change is to make the `Lexer` fully own the memory of the tokens it produces. The Parser and AST will borrow this data.

### 1. `src/Lexer.zig`

The `Lexer` creates `ident` and `string` tokens by allocating new slices on the heap. It must be responsible for freeing them.

-   **`pub fn deinit(self: *Token, alloc: std.mem.Allocator) void`**
    A new method must be added to the `Token` union. It will `switch` on the token type and free the memory for heap-allocated slices.
    ```zig
    // Example implementation
    switch (self.*) {
        .ident => |s| alloc.free(s),
        .string => |s| alloc.free(s),
        else => {},
    }
    ```

-   **`pub fn deinit(self: *Lexer, alloc: std.mem.Allocator) void`**
    The existing `deinit` method is correct for its own memory but must be updated to also deinitialize the contents of its token list.
    ```zig
    // Example implementation
    for (self.tokens.items) |tok| {
        tok.deinit(alloc); // Call deinit on each token
    }
    self.tokens.deinit(alloc);
    self.source_map.deinit(alloc);
    alloc.destroy(self);
    ```

### 2. `src/parser/ast.zig`

This file defines the AST nodes. Recursive `deinit` methods must be added for the main `union` types.

-   **`pub fn deinit(self: *ast.Expression, alloc: std.mem.Allocator) void`**
    This function will `switch` on the expression type and recursively deinitialize all owned memory, such as:
    -   Pointers to other expressions (`*const Expression`).
    -   `ArrayLists` of expressions or other AST nodes (`ArgumentList`, `ast.Block`).
    -   `StringHashMaps` used in struct instantiations.
    -   `Type` nodes.
    -   **Note:** It should **not** free `[]const u8` slices for `.ident` and `.string` variants, as this memory is now owned by the `Lexer`.

-   **`pub fn deinit(self: *ast.Statement, alloc: std.mem.Allocator) void`**
    Similar to `Expression.deinit`, this will `switch` on the statement type and recursively deinitialize all owned memory, including:
    -   `Expression` nodes.
    -   Pointers to other `Statement` nodes.
    -   `ArrayLists` (`ParameterList`, `ast.Block`).
    -   `Type` nodes.
    -   Contained `CompoundTypeDeclaration` and `EnumDeclaration` nodes, which have their own `ArrayLists`.
    -   **Note:** It should **not** free `[]const u8` slices for names (e.g., in `function_definition`), as this memory is owned by the `Lexer`.

-   **`pub fn deinit(self: *ast.Type, alloc: std.mem.Allocator) void`**
    This will `switch` on the AST `Type` union and deinitialize its contents, such as:
    -   Pointers to inner `Type` nodes (`*const Type`).
    -   `ArrayLists` (`ParameterList`, `ArgumentList`).
     -   **Note:** It should **not** free `[]const u8` slices for symbols and names, as this memory is owned by the `Lexer`.

-   **`pub fn deinit(self: *ast.RootNode, alloc: std.mem.Allocator) void`**
    This will be the entry point for deinitializing the entire AST. It will iterate through the `ArrayList(Statement)` and call `deinit` on each statement, before deinitializing the list itself.

### 3. `src/parser/Parser.zig`

The `Parser` struct itself and its sub-parser need updated `deinit` methods.

-   **`pub fn deinit(self: *Parser) void`**
    The existing `deinit` is incomplete. It must be updated to:
    1.  Call the new `ast.RootNode.deinit` on `self.output` to free the entire AST.
    2.  Call `self.type_parser.deinit()`.
    3.  Deinitialize and destroy its hashmaps (`bp_lookup`, `nud_lookup`, etc.).
    4.  Call `self.alloc.destroy(self)` to free the parser object itself.

### 3. `src/parser/TypeParser.zig`

-   **`pub fn deinit(self: *TypeParser) void`**
    A new `deinit` method is required to deinitialize and destroy the hashmaps it owns (`bp_lookup`, `nud_lookup`, `led_lookup`).

## II. Compiler (`src/compiler/`)

The compiler clones the AST and builds its own complex, heap-allocated data structures for types and symbols. These all need to be manually deinitialized.

### 1. `src/compiler/Type.zig`

This is the most critical and complex data structure to deinitialize. The `Type` union and its nested structs are highly recursive and contain many heap-allocated objects.

-   **`pub fn deinit(self: *Type, alloc: std.mem.Allocator) void`**
    A new recursive `deinit` function that `switch`es on the `Type` tag. It must handle:
    -   `.optional`, `.type`: Deinit the inner `*const Type` and free the pointer.
    -   `.reference`, `.slice`, `.array`, `.error_union`: Deinit inner `*const Type` pointers and free them.
    -   `.function`: Call a `Function.deinit` helper.
    -   `.struct`, `.union`, `.enum`: Call a `CompoundType.deinit` helper.

-   **`pub fn deinit(self: *Type.Function, alloc: std.mem.Allocator) void`**
    A new helper function to:
    1.  Deinit all `param.type`s in the `params` and `generic_params` lists.
    2.  Deinit the `params` and `generic_params` `ArrayLists`.
    3.  Deinit and free the `return_type` pointer.
    4.  Free the `args` slice in `generic_instantiation` if it exists.

-   **`pub fn deinit(self: *Type.CompoundType(T), alloc: std.mem.Allocator) void`**
    A new helper function to:
    1.  Deinit `variables`: Iterate and deinit each `Variable.type` and `Variable.value`, then `destroy` the hashmap pointer.
    2.  Deinit `subtypes`: Iterate and deinit each `Subtype.type`, then `destroy` the hashmap pointer.
    3.  Deinit `members`: Iterate and deinit each `MemberType` (if it is a `Type`), then `destroy` the hashmap pointer.
    4.  Deinit `methods`: Iterate and deinit each `Method`'s types, then `destroy` the hashmap pointer.
    5.  Deinit `generic_params` `ArrayList`.
    6.  Deinit and free the `tag_type` pointer.

### 2. `src/compiler/Value.zig`

-   **`pub fn deinit(self: *Value, alloc: std.mem.Allocator) void`**
    A new recursive `deinit` method is required. It will `switch` on the `Value` tag and handle:
    -   `.type`: Call `deinit` on the contained `Type`.
    -   `.function`: Call `deinit` on the `Type.Function`.
    -   `.comptime_struct`:
        1.  Call `deinit` on its `type`.
        2.  Iterate and recursively call `deinit` on each `value` in the `fields` slice.
        3.  Free the `fields` slice itself.

### 3. `src/compiler/Module.zig`

-   **`pub fn deinit(self: *Module, alloc: std.mem.Allocator) void`**
    The existing `deinit` is incorrect. It must be updated to:
    1.  Iterate `self.symbols` and call `deinit` on each `Symbol.type`.
    2.  Call `self.symbols.deinit()` and then `alloc.destroy(self.symbols)`.
    3.  Iterate `self.imports` and recursively call `deinit` on each `Module`.
    4.  Call `self.imports.deinit()` and then `alloc.destroy(self.imports)`.

### 4. `src/compiler/Compiler.zig`

-   **`pub fn deinit(self: *Compiler) void`**
    The existing `deinit` is highly incomplete. It must be updated to:
    1.  **Deinitialize the AST**: Call `ast.RootNode.deinit` on `self.input`.
    2.  **Deinitialize Scopes**: Properly deinitialize all remaining `Scope`s in the `scopes` list. This involves freeing the `pending_defers` list and destroying the `items` hashmap for each scope.
    3.  **Deinitialize `exported_symbols`**: Iterate and deinit `Symbol.type`s, then `deinit` the hashmap.
    4.  **Deinitialize `zag_header_contents`**: Iterate and deinit `Type` keys and free `[]const u8` values, then `deinit` the hashmap.
    5.  **Deinitialize `pending_instantiations`**: Free the `inner_name` and `args` slice for each instantiation.
    6.  The existing deinitializations for `sections`, `type_def_blocks`, etc. should be reviewed for correctness (e.g. freeing vs destroying).
    7.  Finally, call `self.alloc.destroy(self)` to free the compiler object itself.

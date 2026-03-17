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

## III. Lexer, Parser, and Compiler Lifecycle (Deep-Cloning AST)

**Optimal Timeline: Lexer, Parser, and Compiler Lifecycle (Deep-Cloning AST)**

The goal is that the `Compiler` fully owns its AST, independent of the `Parser` and `Lexer`'s lifetimes.

---

**Phase 1: `Compiler.getAST(alloc, file_path)` (Temporary Memory Scope)**

This phase is responsible for taking source code and producing a *deep-cloned, self-contained* Abstract Syntax Tree (AST) that the `Compiler` can then own.

1.  **Read Source File:**
    *   `std.fs.cwd().readFileAlloc(Main_Allocator, file_path)`: Reads the entire source file into a `[]u8` buffer. This buffer is allocated using `getAST`'s `alloc` (which ultimately comes from the `Compiler`'s main allocator).
    *   `owned_path = Main_Allocator.dupe(u8, file_path)`: Duplicates the `file_path` for `Position` objects, ensuring path strings are owned.

2.  **Lexer Initialization:**
    *   `lexer = Lexer.init(file_buffer, Main_Allocator, owned_path)`: Creates a `Lexer` instance.
        *   It stores a borrowed reference to `file_buffer` (it does **not** own or free `file_buffer`).
        *   It receives the **Main Allocator** for its own allocations.
        *   As it tokenizes, for every identifier or string literal, it allocates memory for these `[]const u8` values (e.g., via `toOwnedSlice` or `dupe`) using the **Main Allocator**. These allocations are now "owned" by the `Lexer` object.
        *   It builds its internal `tokens` `ArrayList` using the **Main Allocator**.

3.  **Parser Initialization & AST Construction:**
    *   `parser = Parser.init(lexer, Main_Allocator)`: A `Parser` instance is created.
        *   It takes a reference to the `lexer` (it does **not** own or free the `lexer`).
        *   It receives the **Main Allocator** for its own allocations.
        *   As it builds its AST (`parser.output`), for every `[]const u8` (identifiers, string literals, etc.) it encounters from the tokens, it *must* `Main_Allocator.dupe()` these values into its *own* memory. These are "owned" by the `Parser`.
        *   Any nested AST nodes (`Expression`, `Statement`, `Type` structs) or collections (`ArrayList`, `StringHashMap`) are also allocated using the **Main Allocator** and "owned" by the `Parser`.

4.  **Deep Clone the AST:**
    *   `cloned_ast_root = parser.output.clone(Main_Allocator)`: This is the crucial step. The AST built by the `Parser` (`parser.output`) is **deep-cloned**.
        *   This operation allocates a *completely new, independent copy* of the entire AST structure, including all nested nodes, string slices, and collection elements, all using the **Main Allocator**.
        *   Each `clone` method (e.g., `Statement.clone`, `Expression.clone`, `Type.clone`, `VariableSignature.clone`, `Position.clone`) *must* ensure that any `[]const u8` fields are `Main_Allocator.dupe()`d, and any `*const` pointers are `Main_Allocator.create()`d and filled with cloned content.

5.  **Deinitialize Parser & Lexer (Immediately):**
    *   `defer parser.deinit()`: The `Parser`'s `deinit` method is called. This frees all memory that the `Parser` allocated (its internal hash maps, its `output` AST, and all the `dupe`d strings and nested nodes it owned).
    *   `defer lexer.deinit(Main_Allocator)`: The `Lexer`'s `deinit` method is called. This frees all memory the `Lexer` allocated (its `tokens` `ArrayList` and any `toOwnedSlice`d or `dupe`d string literals it owned).
    *   `Main_Allocator.free(owned_path)`: The duplicated file path string is freed.

6.  **Return Cloned AST & Source:**
    *   `return .{ .root = cloned_ast_root, .source = file_buffer }`: `Compiler.getAST` returns the `cloned_ast_root` and the `file_buffer`. The `file_buffer` is now the responsibility of the caller (the `Compiler` instance).

---

**Phase 2: `Compiler.init(alloc, ast_root, file_path, registry)` (Compiler's Lifetime Scope)**

This phase integrates the deep-cloned AST into the `Compiler` instance.

1.  **Compiler Initialization:**
    *   `compiler = Main_Allocator.create(Compiler)`: The `Compiler` object itself is allocated using the main compilation `allocator` (likely `gpa.allocator()` from `build.zig`).
    *   `compiler.alloc = Main_Allocator`: The `Compiler` explicitly stores the **Main Allocator** as its own for all subsequent internal allocations.
    *   `compiler.input = cloned_ast_root`: The `Compiler` takes ownership of the deep-cloned AST returned by `getAST`.
    *   `compiler.source_path = Main_Allocator.dupe(u8, original_file_path)`: The source file path is duplicated and owned by the `Compiler`.
    *   `compiler.zag_header_contents = std.HashMap.initContext(Main_Allocator, ...)`: All of the `Compiler`'s internal data structures (scopes, symbol tables, type definition blocks, output buffers, etc.) are initialized using `compiler.alloc`. Any `[]const u8` values or other complex types added to these structures during analysis or code generation *must* be `dupe`d or `clone`d using `compiler.alloc`.
    *   `compiler.emit()`: The core compilation logic runs, operating on `compiler.input` (the cloned AST) and populating `compiler`'s internal structures.

---

**Phase 3: `compiler.deinit()` (Compiler's End-of-Life)**

When the `Compiler` is finished with its work (e.g., after `compiler.emit()` completes), it cleans up all its owned memory.

1.  **Deinitialize Compiler Internal Structures:**
    *   `compiler.deinit()` is called (typically via `defer`).
    *   All of `compiler`'s internal `ArrayList`s, `StringHashMap`s, `EnumArray`s, etc., have their `deinit` methods called. This includes:
        *   Freeing all elements they contain (which themselves might involve further `deinit` calls for complex types or `alloc.free()` for `dupe`d strings).
        *   Freeing the backing memory of the collections themselves.
    *   `Main_Allocator.free(compiler.source_path)`: The compiler's owned `source_path` is freed.

2.  **Deinitialize Compiler-Owned AST:**
    *   `compiler.input.deinit(Main_Allocator)`: The `ast.RootNode.deinit` method is called on the cloned AST. This (a function that would need to be implemented) recursively deinitializes all `Statement`s, `Expression`s, `Type`s, and any `dupe`d strings within the cloned AST, releasing all the memory that was allocated for it by the **Main Allocator**.

3.  **Deinitialize Compiler Object:**
    *   `Main_Allocator.destroy(compiler)`: The `Compiler` object itself is freed.

---

**Summary of Key Memory Principles:**

*   **Ownership:** Each component (Lexer, Parser, Compiler) is responsible for `deinit`ializing only the memory that it *allocated*.
*   **Deep Copy at Transfer:** When ownership of a complex data structure (like the AST) is transferred from one component/allocator to another, a *deep copy* (or deep clone) is performed.
*   **Borrowing vs. Owning:** Be explicit whether a component is borrowing a reference to data (and thus not responsible for freeing it) or owning it (and thus responsible for freeing it). In this case, the `Compiler` *owns* its AST.
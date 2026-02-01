# Type Generics in Zag

Zag implements type generics using a **monomorphization** strategy, similar to C++ templates or Zig's `comptime`. This means that generic types and functions are compile-time templates. When you use a generic type with specific arguments (e.g., `Box<i32>`), the compiler generates a specialized, concrete version of that type (e.g., `Box_i32`) optimized for those exact arguments.

## Declaring Generics

Generics can be defined for **Structs**, **Unions**, and **Functions** using angle brackets `<...>`. Zag supports both **Type Parameters** and **Non-Type Parameters (Const Generics)**.

### Generic Structs
```zag
struct Box<T> {
    content: T,
}

// Non-type parameters (const generics)
struct Array<T, N: u32> {
    data: [N]T,
}
```

### Generic Functions
```zag
fn identity<T>(value: T) T {
    return value;
}

// Using constants in functions
fn repeat<T, N: u32>(value: T) void {
    // ...
}
```

### Generic Methods
You can define methods on generic structs that also use the generic parameters. Zag correctly handles self-referential generic types in method signatures.
```zag
struct List<T> {
    data: &mut T,
    size: usize,

    fn get(self: List<T>, index: usize) T {
        return self.data[index];
    }
}
```

## Instantiation

Generics are instantiated explicitly by providing type or value arguments.

```zag
fn main() i32 {
    // Instantiate a generic struct with a type
    let b = Box<i32>{ content: 42 };

    // Instantiate with a constant (const generic)
    let arr = Array<i32, 10>{ ... };

    // Instantiate and call a generic function
    let x = identity<f32>(3.14);
}
```

## How It Works Internally

The Zag compiler handles generics in several phases:

1.  **Parsing**: The parser recognizes `<...>` syntax in type declarations, signatures, and expressions.
2.  **Inference & Instantiation**: When the compiler encounters a generic usage like `Box<i32>` or `Array<i32, 3>`:
    *   It evaluates the arguments at compile-time (resolving types and constant values).
    *   It generates a unique **mangled name** based on the base name and the hash of the arguments (e.g., `Box_151046...` or `Array_i32_3`).
    *   It checks a global cache to see if this specialization already exists.
    *   If it's new, it creates a copy of the original AST definition, binding the generic parameters to the concrete values in a temporary scope, and queues it for compilation.
3.  **Compilation (Monomorphization)**: The compiler processes the queued specializations, generating standard C structs and functions for each one.

### Recursive and Self-Referential Types
The system supports complex recursive definitions. If a generic struct method refers back to its parent type (e.g., `fn f(self: Box<T>)`), the compiler correctly resolves the instantiation to break recursion cycles.

### Cross-Module Support
Generics work seamlessly across modules. When you import a module and use its generic types, the compiler correctly restores the original module's scope to resolve internal symbols (like `malloc` in a library) while binding your provided arguments.

## Built-in Generics

The language provides built-in support for certain generic-like operations, most notably `sizeof`.

```zag
// Returns the size of the type T in bytes
let s = sizeof(i32); 
```
Unlike standard functions, `sizeof` takes a `type` as a value argument (enabled by the same underlying const generic infrastructure).

## Current Limitations

*   **Explicit Instantiation**: You must currently provide arguments explicitly (e.g., `identity<i32>(10)`). Implicit inference from call-site arguments (e.g., `identity(10)`) is planned for the future.
*   **Nested Generics**: Parsing of nested generics like `Box<List<T>>` may require parentheses or whitespace to avoid ambiguity with the `>>` operator in some contexts.

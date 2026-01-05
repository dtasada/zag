pub const CONTENT =
    \\#ifndef ZAG_H
    \\#define ZAG_H
    \\
    \\#include <stdbool.h>
    \\#include <stddef.h>
    \\#include <stdint.h>
    \\
    \\typedef int8_t  i8;
    \\typedef int16_t i16;
    \\typedef int32_t i32;
    \\typedef int64_t i64;
    \\
    \\typedef uint8_t  u8;
    \\typedef uint16_t u16;
    \\typedef uint32_t u32;
    \\typedef uint64_t u64;
    \\
    \\typedef size_t usize;
    \\typedef char   c_char;
    \\typedef int    c_int;
    \\
    \\typedef float  f32;
    \\typedef double f64;
    \\
    \\#define __ZAG_OPTIONAL_TYPE(union_name, T) \
    \\    typedef struct {                       \
    \\        bool is_some;                      \
    \\        T    payload;                      \
    \\    } union_name;
    \\
    \\#define __ZAG_ERROR_UNION_TYPE(union_name, error_type, success_type) \
    \\    typedef struct {                                                 \
    \\        bool is_success;                                             \
    \\        union {                                                      \
    \\            success_type success;                                    \
    \\            error_type   error;                                      \
    \\        } payload;                                                   \
    \\    } union_name;
    \\
    \\#define __ZAG_ARRAYLIST_DEF(name, T) \
    \\    typedef struct {                 \
    \\        T    *items;                 \
    \\        usize capacity;              \
    \\        usize len;                   \
    \\    } name;
    \\
    \\#define __ZAG_ARRAYLIST_IMPL(name, T)                                  \
    \\    name name##_create(void) {                                         \
    \\        return (name){                                                 \
    \\            .items    = malloc(sizeof(T) * 16),                        \
    \\            .capacity = 16,                                            \
    \\            .len      = 0,                                             \
    \\        };                                                             \
    \\    }                                                                  \
    \\                                                                       \
    \\    void name##_destroy(name *self) {                                  \
    \\        free(self->items);                                             \
    \\        self->items    = NULL;                                         \
    \\        self->capacity = 0;                                            \
    \\        self->len      = 0;                                            \
    \\    }                                                                  \
    \\                                                                       \
    \\    void name##_append(name *self, T item) {                           \
    \\        if (self->len == self->capacity) {                             \
    \\            usize new_cap = self->capacity ? self->capacity * 2 : 16;  \
    \\            void *ptr     = realloc(self->items, new_cap * sizeof(T)); \
    \\            if (!ptr) abort();                                         \
    \\            self->items    = ptr;                                      \
    \\            self->capacity = new_cap;                                  \
    \\        }                                                              \
    \\        self->items[self->len++] = item;                               \
    \\    }
    \\
;

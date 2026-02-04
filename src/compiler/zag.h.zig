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
    \\#define c_null ((void*)0)
    \\
    \\#define __ZAG_OPTIONAL_TYPE(union_name, T) \
    \\    typedef struct {                       \
    \\        bool is_some;                      \
    \\        T    payload;                      \
    \\    } union_name;
    \\
    \\#define __ZAG_ERROR_UNION_TYPE(union_name, failure_type, success_type) \
    \\    typedef struct {                                                 \
    \\        bool is_success;                                             \
    \\        union {                                                      \
    \\            success_type success;                                    \
    \\            failure_type failure;                                    \
    \\        } payload;                                                   \
    \\    } union_name;
    \\
    \\#define __ZAG_SLICE_TYPE(name, T) \
    \\    typedef struct {              \
    \\        T    *ptr;                \
    \\        usize len;                \
    \\    } name;
    \\
    \\#define __ZAG_SLICE_SLICE(type_name, slice, start, end) \
    \\    (type_name){ .ptr = &((slice).ptr[start]), .len = (end) - (start) }
    \\
    \\#define __ZAG_SLICE_ARRAY(type_name, slice, start, end) \
    \\    (type_name){ .ptr = &((slice)[start]), .len = (end) - (start) }
    \\
;

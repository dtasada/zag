#ifndef ZAG_H
#define ZAG_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef int8_t  i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef size_t size;

typedef float  f32;
typedef double f64;

#define __ZAG_OPTIONAL_TYPE(union_name, inner_type) \
    typedef struct {                                \
        bool       is_some;                         \
        inner_type payload;                         \
    } union_name;

#define __ZAG_ERROR_UNION_TYPE(union_name, error_type, success_type) \
    typedef struct {                                                 \
        bool is_success;                                             \
        union {                                                      \
            success_type success;                                    \
            error_type   error;                                      \
        } payload;                                                   \
    } union_name;

#endif

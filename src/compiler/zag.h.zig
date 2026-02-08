pub const CONTENT =
    \\#ifndef ZAG_H
    \\#define ZAG_H
    \\
    \\#include <stdbool.h>
    \\#include <stddef.h>
    \\#include <stdint.h>
    \\#include <stdlib.h>
    \\
    \\#define __ZAG_SLICE_SLICE(type_name, slice, start, end) \
    \\    (type_name){ .ptr = &((slice).ptr[start]), .len = (end) - (start) }
    \\
    \\#define __ZAG_SLICE_ARRAY(type_name, slice, start, end) \
    \\    (type_name){ .ptr = &((slice)[start]), .len = (end) - (start) }
    \\
;

/*
 * ERROR -- Error reporting utilities.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <stdarg.h>
#include <stddef.h>

#include <a3/str.h>

typedef struct Span {
    size_t    line;
    A3CString text;
} Span;

void verror_at(A3CString src, Span, char* fmt, va_list);
void error_at(A3CString src, Span, char* fmt, ...);

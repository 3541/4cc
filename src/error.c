/*
 * ERROR -- Error reporting utilities.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include "error.h"

#include <stdarg.h>
#include <stdio.h>

#include <a3/str.h>
#include <a3/util.h>

A3_FORMAT_FN(2, 0)
static void verror_at_eof(A3CString src, char* fmt, va_list args) {
    assert(src.ptr);

    fputs("Error: At EOF: ", stderr);
    vfprintf(stderr, fmt, args);
    fputc('\n', stderr);
}

A3_FORMAT_FN(3, 0)
void verror_at(A3CString src, Span span, char* fmt, va_list args) {
    assert(src.ptr);

    if (!span.text.ptr) {
        verror_at_eof(src, fmt, args);
        return;
    }

    assert(a3_string_cptr(span.text) >= a3_string_cptr(src));

    A3CString line = span.text;
    while (line.ptr > src.ptr && *line.ptr != '\n') {
        line.ptr--;
        line.len++;
    }
    if (*line.ptr == '\n') {
        line.ptr++;
        line.len--;
    }

    while (line.ptr[line.len - 1] != '\n' && line.len < src.len)
        line.len++;
    if (line.ptr[line.len - 1] == '\n')
        line.len--;

    int offset = (int)(a3_string_cptr(span.text) - a3_string_cptr(line));
    offset += fprintf(stderr, "Error (%zu): ", span.line);
    fprintf(stderr,
            A3_S_F "\n"
                   "%*s",
            A3_S_FORMAT(line), offset, "");
    for (size_t i = 0; i < a3_string_len(span.text); i++)
        fputc('^', stderr);
    fputc(' ', stderr);

    vfprintf(stderr, fmt, args);
    fputc('\n', stderr);
}

A3_FORMAT_FN(3, 4)
void error_at(A3CString src, Span span, char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    verror_at(src, span, fmt, args);
    va_end(args);
}

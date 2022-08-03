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

    size_t eol_count = 0;
    for (size_t i = 0; i < span.text.len; i++)
        eol_count += span.text.ptr[i] == '\n';

    // Spanned area, expanded to previous and following EOL.
    A3CString chunk = span.text;
    while (chunk.ptr > src.ptr && *chunk.ptr != '\n') {
        chunk.ptr--;
        chunk.len++;
    }
    if (*chunk.ptr == '\n') {
        chunk.ptr++;
        chunk.len--;
    }

    while (chunk.ptr[chunk.len - 1] != '\n' && chunk.len < src.len)
        chunk.len++;
    if (chunk.ptr[chunk.len - 1] == '\n')
        chunk.len--;

    fprintf(stderr, "Error (%zu):\n", span.line);
    do {
        size_t offset = 0;
        while (isspace(chunk.ptr[offset]) && chunk.ptr[offset] != '\n')
            offset++;

        size_t    len      = 0;
        ptrdiff_t next_eol = (uint8_t*)memchr(chunk.ptr, '\n', chunk.len) - chunk.ptr;
        if (next_eol >= 0)
            len = (size_t)next_eol;
        else
            len = chunk.len;

        A3CString whitespace = { .ptr = chunk.ptr, .len = offset };
        A3CString line       = { .ptr = chunk.ptr, .len = len };
        fprintf(stderr, A3_S_F "\n" A3_S_F, A3_S_FORMAT(line), A3_S_FORMAT(whitespace));

        if (line.ptr + line.len >= span.text.ptr) {
            int caret_offset = line.ptr < span.text.ptr
                                   ? (int)((size_t)(span.text.ptr - line.ptr) - whitespace.len)
                                   : 0;
            assert(caret_offset >= 0);
            fprintf(stderr, "%*s", caret_offset, "");
            for (size_t j = 0; j < len - offset - (size_t)caret_offset &&
                               line.ptr + j + caret_offset < span.text.ptr + span.text.len - 1;
                 j++)
                fputc('^', stderr);

            if (line.ptr + line.len >= span.text.ptr + span.text.len) {
                fputc(' ', stderr);
                vfprintf(stderr, fmt, args);
            }

            fputc('\n', stderr);
        }

        chunk.ptr += line.len + 1;
        chunk.len -= line.len + 1;
    } while (eol_count-- > 0);
}

A3_FORMAT_FN(3, 4)
void error_at(A3CString src, Span span, char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    verror_at(src, span, fmt, args);
    va_end(args);
}

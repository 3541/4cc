/*
 * 4CC -- C compiler.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <a3/buffer.h>
#include <a3/log.h>
#include <a3/str.h>

#include "ast.h"
#include "dump.h"
#include "error.h"
#include "gen.h"
#include "lex.h"
#include "parse.h"
#include "type.h"

static A3CString file_read(char const* path) {
    assert(path);

    FILE* file = fopen(path, "r");
    if (!file) {
        fputs("Failed to open file.\n", stderr);
        return A3_CS_NULL;
    }

    A3Buffer buf = { .data = A3_S_NULL };
    a3_buf_init(&buf, 1024, 1024ULL * 1024 * 1024);
    while (!feof(file) && !ferror(file)) {
        a3_buf_ensure_cap(&buf, 1024);
        A3String space = a3_buf_write_ptr(&buf);
        size_t   read  = fread(space.ptr, 1, space.len, file);
        a3_buf_wrote(&buf, read);
    }

    if (ferror(file)) {
        fputs("Error reading file.\n", stderr);
        return A3_CS_NULL;
    }

    return a3_buf_read_ptr(&buf);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <FILE>\n", argv[0]);
        return -1;
    }

    a3_log_init(stderr, A3_LOG_INFO);

    A3CString src = file_read(argv[1]);
    if (!src.ptr)
        return -1;

    Lexer*  lexer  = lex_new(src);
    Parser* parser = parse_new(src, lexer);

    Vertex* root = parse(parser);
    if (!root)
        return -1;

    Registry* reg = type_registry_new();
    if (!type(reg, src, root))
        return -1;

    dump(root);

    if (!gen(src, root))
        return -1;

    return 0;
}

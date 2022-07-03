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
#include "config.h"
#include "dump.h"
#include "error.h"
#include "gen.h"
#include "lex.h"
#include "parse.h"
#include "type.h"

static A3CString file_read(A3CString path) {
    assert(path.ptr);

    FILE* file = fopen(a3_string_cstr(path), "r");
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

static void usage(char const* name, int status) {
    assert(name);

    fprintf(stderr, "Usage: %s [-o <FILE>] <FILE>\n", name);
    exit(status);
}

static Config arg_parse(size_t argc, char const* argv[]) {
    if (argc < 2)
        usage(argv[0], -1);

    Config ret = { 0 };

    for (size_t i = 1; i < argc; i++) {
        if (*argv[i] != '-') {
            if (ret.src.ptr) {
                fprintf(stderr, "Multiple source files specified.\n");
                exit(-1);
            }

            ret.src = a3_cstring_from(argv[i]);
            continue;
        }

        switch (argv[i][1]) {
        case 'o':
            if (ret.out_path.ptr) {
                fprintf(stderr, "-o specified multiple times.\n");
                exit(-1);
            }

            if (argv[i][2]) {
                ret.out_path = a3_cstring_from(&argv[i][2]);
            } else {
                if (i + 1 >= argc || !*argv[i + 1]) {
                    fprintf(stderr, "Missing output file.\n");
                    exit(-1);
                }

                ret.out_path = a3_cstring_from(argv[++i]);
            }

            break;
        case 'h':
            usage(argv[0], 0);
            break;
        default:
            fprintf(stderr, "Unrecognized option \"%s\".\n", argv[i]);
            usage(argv[0], -1);
        }
    }

    return ret;
}

int main(int argc, char const* argv[]) {
    a3_log_init(stderr, A3_LOG_INFO);

    Config cfg = arg_parse((size_t)argc, argv);

    A3CString src = file_read(cfg.src);
    if (!src.ptr)
        return -1;

    cfg.out = stdout;
    if (cfg.out_path.ptr)
        cfg.out = fopen(a3_string_cstr(cfg.out_path), "w");
    if (!cfg.out) {
        fprintf(stderr, "Failed to open output file.\n");
        return -1;
    }

    Lexer*  lexer  = lex_new(src);
    Parser* parser = parse_new(src, lexer);

    Vertex* root = parse(parser);
    if (!root)
        return -1;

    Registry* reg = type_registry_new();
    if (!type(reg, src, root))
        return -1;

    dump(root);

    if (!gen(&cfg, src, root))
        return -1;

    return 0;
}

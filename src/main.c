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
#include <libgen.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <a3/buffer.h>
#include <a3/log.h>
#include <a3/str.h>
#include <a3/vec.h>

#include "ast.h"
#include "config.h"
#include "dump.h"
#include "error.h"
#include "gen.h"
#include "lex.h"
#include "parse.h"
#include "subprocess.h"
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

    fprintf(stderr, "Usage: %s [-hcSE] [-o <FILE>] <FILE>\n", name);
    exit(status);
}

static Config arg_parse(size_t argc, char const* argv[]) {
    if (argc < 2)
        usage(argv[0], -1);

    Config ret = { 0 };
    A3_VEC_INIT(A3CString, &ret.preprocess_args);

    for (size_t i = 1; i < argc; i++) {
        if (*argv[i] != '-') {
            if (ret.src_path.ptr) {
                fprintf(stderr, "Multiple source files specified.\n");
                exit(-1);
            }

            ret.src_path = a3_cstring_from(argv[i]);
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
        case 'c':
            ret.output_obj = true;
            break;
        case 'S':
            ret.output_asm = true;
            break;
        case 'E':
            ret.output_preprocessed = true;
            break;
        case 'I': {
            A3CString flag = a3_cstring_from(argv[i]);
            A3_VEC_PUSH(&ret.preprocess_args, &flag);

            if (!argv[i][2]) {
                if (i + 1 >= argc || !*argv[i + 1]) {
                    fprintf(stderr, "Missing path argument to -I.\n");
                    exit(-1);
                }

                A3CString path = a3_cstring_from(argv[++i]);
                A3_VEC_PUSH(&ret.preprocess_args, &path);
            }

            break;
        }
        case 'C':
            A3_VEC_PUSH(&ret.preprocess_args, &A3_CS("-C"));
            break;
        default:
            if (strcmp(argv[i], "--dump-ast") == 0) {
                ret.dump_ast = true;
                break;
            }
            if (strcmp(argv[i], "--preserve-tmpfiles") == 0) {
                ret.keep_tmp = true;
                break;
            }

            fprintf(stderr, "Unrecognized option \"%s\".\n", argv[i]);
            usage(argv[0], -1);
        }
    }

    return ret;
}

static A3CString make_file(A3CString path, A3CString name, A3CString ext) {
    assert(path.ptr);
    assert(name.ptr);
    assert(ext.ptr);

    A3String ret = a3_string_alloc(path.len + name.len + ext.len + 2);
    a3_string_concat(ret, 4, path, A3_CS("/"), name, ext);

    return A3_S_CONST(ret);
}

static A3CString append(A3CString s1, A3CString s2) {
    assert(s1.ptr);
    assert(s2.ptr);

    A3String ret = a3_string_alloc(s1.len + s2.len + 1);
    a3_string_concat(ret, 2, s1, s2);

    return A3_S_CONST(ret);
}

static A3CString path_for(Config const* cfg, A3CString extension, bool is_final) {
    assert(cfg);
    assert(extension.ptr);

    A3CString ret;

    if (is_final) {
        if (cfg->out_path.ptr)
            ret = cfg->out_path;
        else
            ret = append(cfg->src_name, extension);
    } else {
        ret = make_file(cfg->tmp_dir, cfg->src_name, extension);
    }

    return ret;
}

static void config_init(Config* cfg) {
    assert(cfg);

    A3String path_copy = a3_string_clone(cfg->src_path);
    cfg->src_name      = a3_cstring_from(basename((char*)path_copy.ptr));

    A3String tmp_dir = a3_string_clone(A3_CS("/tmp/4ccXXXXXX"));
    if (!mkdtemp((char*)tmp_dir.ptr)) {
        perror("mkdtemp");
        exit(-1);
    }
    cfg->tmp_dir = A3_S_CONST(tmp_dir);

    cfg->preprocess_out_path = path_for(cfg, A3_CS(".pre"), cfg->output_preprocessed);
    cfg->asm_out_path        = path_for(cfg, A3_CS(".asm"), cfg->output_asm);
    cfg->obj_out_path        = path_for(cfg, A3_CS(".o"), cfg->output_obj);

    if (a3_string_cmp(cfg->asm_out_path, A3_CS("-")) == 0)
        cfg->asm_out = stdout;
    else
        cfg->asm_out = fopen(a3_string_cstr(cfg->asm_out_path), "w");
    if (!cfg->asm_out) {
        fprintf(stderr, "Failed to open output \"" A3_S_F "\".\n", A3_S_FORMAT(cfg->asm_out_path));
        exit(-1);
    }
}

static void compile(Config const* cfg) {
    assert(cfg);

    A3CString src = file_read(cfg->preprocess_out_path);
    if (!src.ptr) {
        fprintf(stderr, "Failed to read preprocessed source.\n");
        exit(-1);
    }

    Lexer*  lexer  = lex_new(src);
    Parser* parser = parse_new(src, lexer);

    Vertex* root = parse(parser);
    if (!root) {
        fprintf(stderr, "Parsing failed.\n");
        exit(-1);
    }

    Registry* reg = type_registry_new();
    if (!type(reg, src, root)) {
        fprintf(stderr, "Typechecking failed.\n");
        exit(-1);
    }

    if (cfg->dump_ast)
        dump(root);

    if (!gen(cfg, src, root)) {
        fprintf(stderr, "Codegen failed.\n");
        exit(-1);
    }
}

static void cleanup(Config const* cfg) {
    assert(cfg);

    if (cfg->keep_tmp)
        return;

    if (!cfg->output_preprocessed)
        remove(a3_string_cstr(cfg->preprocess_out_path));
    if (!cfg->output_asm)
        remove(a3_string_cstr(cfg->asm_out_path));
    if (!cfg->output_obj)
        remove(a3_string_cstr(cfg->obj_out_path));
    remove(a3_string_cstr(cfg->tmp_dir));
}

int main(int argc, char const* argv[]) {
    a3_log_init(stderr, A3_LOG_INFO);

    Config cfg = arg_parse((size_t)argc, argv);
    config_init(&cfg);

    preprocess(cfg.src_path, cfg.preprocess_out_path, &cfg.preprocess_args);
    if (cfg.output_preprocessed)
        goto done;

    compile(&cfg);
    if (cfg.output_asm)
        goto done;

    assemble(cfg.asm_out_path, cfg.obj_out_path);

done:
    cleanup(&cfg);

    return 0;
}

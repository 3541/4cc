/*
 * 4CC -- C compiler.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include <assert.h>
#include <fcntl.h>
#include <libgen.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <a3/log.h>
#include <a3/str.h>
#include <a3/vec.h>

#include "config.h"
#include "dump.h"
#include "gen.h"
#include "lex.h"
#include "parse.h"
#include "subprocess.h"
#include "type.h"

static A3CString file_read(A3CString path) {
    assert(path.ptr);

    int file = open(a3_string_cstr(path), O_RDONLY);
    if (file < 0) {
        perror("open");
        exit(-1);
    }

    struct stat st;
    if (fstat(file, &st) < 0) {
        perror("fstat");
        exit(-1);
    }

    void* buf = mmap(NULL, (size_t)st.st_size, PROT_READ, MAP_PRIVATE, file, 0);
    if (buf == MAP_FAILED) {
        perror("mmap");
        exit(-1);
    }

    return a3_cstring_new(buf, (size_t)st.st_size);
}

static void usage(char const* name, int status) {
    assert(name);

    fprintf(stderr,
            "Usage: %s [-hcCSE] [--dump-ast] [--preserve-tmpfiles] [-I <FILE>] [-o <FILE>] "
            "<FILES...>\n",
            name);
    exit(status);
}

static A3CString append(A3CString s1, A3CString s2) {
    assert(s1.ptr);
    assert(s2.ptr);

    A3String ret = a3_string_alloc(s1.len + s2.len + 1);
    a3_string_concat(ret, 2, s1, s2);

    return A3_S_CONST(ret);
}

static void preprocess_args_init(char const* bin, A3Vec* args) {
    assert(bin);
    assert(args);

    A3_VEC_INIT(A3CString, args);

    A3String prefix = a3_string_clone(a3_cstring_from(bin));
    prefix.len -= a3_string_rchr(prefix, '/').len;

    A3CString path = append(A3_S_CONST(prefix), A3_CS("/include"));

    A3_VEC_PUSH(args, &A3_CS("-I"));
    A3_VEC_PUSH(args, &path);

    A3_VEC_PUSH(args, &A3_CS("-undef"));
    A3_VEC_PUSH(args, &A3_CS("-U_FORTIFY_SOURCE"));
    A3_VEC_PUSH(args, &A3_CS("-U__STDC_VERSION__"));
    A3_VEC_PUSH(args, &A3_CS("-D__x86_64__"));
    A3_VEC_PUSH(args, &A3_CS("-Dfloat=int"));
    A3_VEC_PUSH(args, &A3_CS("-Ddouble=long"));
}

static bool string_ends_with(A3CString s, A3CString suffix) {
    assert(s.ptr);
    assert(suffix.ptr);

    A3CString s1 = a3_cstring_new(s.ptr + s.len - suffix.len, suffix.len);
    return a3_string_cmp(s1, suffix) == 0;
}

static FileType file_type(A3CString name) {
    assert(name.ptr);

    if (string_ends_with(name, A3_CS(".c")))
        return FILE_SRC;
    if (string_ends_with(name, A3_CS(".asm")))
        return FILE_ASM;
    if (string_ends_with(name, A3_CS(".o")))
        return FILE_OBJ;

    fprintf(stderr, "Unrecognized file type " A3_S_F ".\n", A3_S_FORMAT(name));
    exit(-1);
}

static Config arg_parse(size_t argc, char const* argv[]) {
    if (argc < 2)
        usage(argv[0], -1);

    Config ret = { 0 };
    preprocess_args_init(argv[0], &ret.preprocess_args);
    A3_VEC_INIT(File, &ret.files);

    for (size_t i = 1; i < argc; i++) {
        if (*argv[i] != '-') {
            A3CString path     = a3_cstring_from(argv[i]);
            A3String  name_tmp = a3_string_clone(path);
            A3CString name     = a3_cstring_from(basename((char*)name_tmp.ptr));

            A3_VEC_PUSH(&ret.files,
                        (&(File) { .path = path, .name = name, .type = file_type(name) }));

            continue;
        }

        if (strcmp(argv[i], "-ansi") == 0 || strcmp(argv[i], "-pedantic") == 0 ||
            strcmp(argv[i], "-no-pie") == 0)
            continue;

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
        case 'O':
        case 'W':
        case 'g':
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

    if ((ret.output_preprocessed || ret.output_asm || ret.output_obj) && ret.files.len > 1) {
        fprintf(stderr, "-E, -c, and -S cannot be used with multiple source files.");
        exit(-1);
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

static void config_init(Config* cfg) {
    assert(cfg);

    A3String tmp_dir = a3_string_clone(A3_CS("/tmp/4ccXXXXXX"));
    if (!mkdtemp((char*)tmp_dir.ptr)) {
        perror("mkdtemp");
        exit(-1);
    }
    cfg->tmp_dir = A3_S_CONST(tmp_dir);

    if (!cfg->out_path.ptr) {
        A3CString name = A3_VEC_AT(File, &cfg->files, 0)->name;

        if (cfg->output_preprocessed)
            cfg->out_path = append(name, A3_CS(".pre.c"));
        else if (cfg->output_asm)
            cfg->out_path = append(name, A3_CS(".asm"));
        else if (cfg->output_obj)
            cfg->out_path = append(name, A3_CS(".o"));
        else
            cfg->out_path = A3_CS("a.out");
    } else if (a3_string_cmp(cfg->out_path, A3_CS("-")) == 0 &&
               (!cfg->output_preprocessed && !cfg->output_asm)) {
        fprintf(stderr, "Output to stdout requires either -E or -S.");
        exit(-1);
    }
}

static void file_process(Config* cfg, File* file) {
    assert(cfg);

    A3CString as = A3_CS_NULL;
    if (file->type == FILE_SRC) {
        A3CString preprocessed = make_file(cfg->tmp_dir, file->name, A3_CS(".pre.c"));
        preprocess(file->path, preprocessed, &cfg->preprocess_args);

        if (cfg->output_preprocessed) {
            file->out = preprocessed;
            return;
        }

        A3CString src = file_read(preprocessed);
        remove(a3_string_cstr(preprocessed));
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

        as = make_file(cfg->tmp_dir, file->name, A3_CS(".asm"));
        if (!gen(cfg, file, src, as, root)) {
            fprintf(stderr, "Codegen failed.\n");
            exit(-1);
        }

        if (cfg->output_asm) {
            file->out = as;
            return;
        }
    } else if (file->type == FILE_ASM) {
        as = file->path;
    }

    A3CString obj = A3_CS_NULL;
    if (file->type == FILE_SRC || file->type == FILE_ASM) {
        obj = make_file(cfg->tmp_dir, file->name, A3_CS(".o"));
        assemble(as, obj);

        if (file->type != FILE_ASM)
            remove(a3_string_cstr(as));
    } else {
        obj = file->path;
    }

    file->out = obj;
}

static void cleanup(Config* cfg) {
    assert(cfg);

    if (cfg->keep_tmp)
        return;

    if (!cfg->output_asm && !cfg->output_preprocessed && !cfg->output_obj) {
        A3_VEC_FOR_EACH (File, file, &cfg->files) {
            if (file->type != FILE_OBJ)
                remove(a3_string_cstr(file->out));
        }
    }

    remove(a3_string_cstr(cfg->tmp_dir));
}

static void copy_to(A3CString from, A3CString to) {
    uint8_t buf[8192] = {};

    FILE* in  = fopen(a3_string_cstr(from), "r");
    FILE* out = a3_string_cmp(to, A3_CS("-")) == 0 ? stdout : fopen(a3_string_cstr(to), "w");

    if (!in || !out) {
        fprintf(stderr, "Failed to open one of " A3_S_F " or " A3_S_F ".\n", A3_S_FORMAT(from),
                A3_S_FORMAT(to));
        exit(-1);
    }

    size_t n = 0;
    while ((n = fread(buf, 1, sizeof(buf), in)) && !ferror(in) && !ferror(out)) {
        while (n > 0 && !ferror(out))
            n -= fwrite(buf, 1, n, out);
    }

    if (ferror(in) || ferror(out) || n) {
        fprintf(stderr, "Failed to copy from " A3_S_F " to " A3_S_F ".\n", A3_S_FORMAT(from),
                A3_S_FORMAT(to));
        exit(-1);
    }
}

static void move_to(A3CString from, A3CString to) {
    if (a3_string_cmp(to, A3_CS("-")) == 0 ||
        rename(a3_string_cstr(from), a3_string_cstr(to)) != 0) {
        copy_to(from, to);
    }
}

int main(int argc, char const* argv[]) {
    a3_log_init(stderr, A3_LOG_INFO);

    Config cfg = arg_parse((size_t)argc, argv);
    config_init(&cfg);

    A3_VEC_FOR_EACH (File, file, &cfg.files)
        file_process(&cfg, file);

    A3CString out = A3_VEC_AT(File, &cfg.files, 0)->out;
    if (cfg.output_preprocessed || cfg.output_asm || cfg.output_obj) {
        move_to(out, cfg.out_path);
        goto done;
    }

    A3Vec objs;
    A3_VEC_INIT(A3CString, &objs);

    A3_VEC_FOR_EACH (File, file, &cfg.files)
        A3_VEC_PUSH(&objs, &file->out);
    link(&objs, cfg.out_path);

done:
    cleanup(&cfg);

    return 0;
}

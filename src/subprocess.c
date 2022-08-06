/*
 * SUBPROCESS -- Subprocess execution facilities.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include "subprocess.h"

#include <assert.h>
#include <errno.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>

#include <a3/str.h>
#include <a3/vec.h>

extern char** environ;

static char* cstring_clone(A3CString s) {
    assert(s.ptr);

    A3String ret = a3_string_alloc(s.len + 1);
    a3_string_concat(ret, 2, s, A3_CS("\0"));

    return (char*)ret.ptr;
}

static void subprocess_run(char* const argv[]) {
    assert(argv);

    pid_t pid;
    int   res = posix_spawnp(&pid, argv[0], NULL, NULL, argv, environ);
    if (res != 0) {
        errno = res;
        perror("posix_spawnp");
        exit(-1);
    }

    int status;
    if (waitpid(pid, &status, 0) < 0) {
        perror("waitpid");
        exit(-1);
    }

    if (!WIFEXITED(status)) {
        fprintf(stderr, "Subprocess %s exited abnormally.\n", argv[0]);
        exit(-1);
    }

    if (WEXITSTATUS(status) != 0) {
        fprintf(stderr, "Subprocess %s exited with status %d.\n", argv[0], WEXITSTATUS(status));
        exit(-1);
    }
}

void preprocess(A3CString src, A3CString dst, A3Vec* args) {
    assert(src.ptr);
    assert(dst.ptr);

    A3Vec final_args;
    A3_VEC_INIT(char*, &final_args);

    A3_VEC_PUSH(&final_args, &(char*) { "cc" });
    A3_VEC_PUSH(&final_args, &(char*) { "-E" });
    A3_VEC_PUSH(&final_args, &(char*) { "-P" });
    A3_VEC_FOR_EACH (A3CString, arg, args)
        A3_VEC_PUSH(&final_args, arg);

    if (a3_string_cmp(dst, A3_CS("-")) != 0) {
        A3_VEC_PUSH(&final_args, &(char*) { "-o" });
        A3_VEC_PUSH(&final_args, &(char*) { cstring_clone(dst) });
    }

    A3_VEC_PUSH(&final_args, &(char*) { cstring_clone(src) });
    A3_VEC_PUSH(&final_args, &(char*) { NULL });

    subprocess_run(final_args.buf);
}

void assemble(A3CString src, A3CString dst) {
    assert(src.ptr);
    assert(dst.ptr);

    subprocess_run(
        (char* const[]) { "nasm", "-felf64", "-o", cstring_clone(dst), cstring_clone(src), NULL });
}

void link(A3Vec* srcs, A3CString dst) {
    assert(srcs);
    assert(dst.ptr);

    A3Vec final_args;
    A3_VEC_INIT(char*, &final_args);

    A3_VEC_PUSH(&final_args, &(char*) { "cc" });
    A3_VEC_PUSH(&final_args, &(char*) { "-static" });
    A3_VEC_PUSH(&final_args, &(char*) { "-o" });
    A3_VEC_PUSH(&final_args, &(char*) { cstring_clone(dst) });

    A3_VEC_FOR_EACH (A3CString, src, srcs)
        A3_VEC_PUSH(&final_args, &(char*) { (char*)src->ptr });

    A3_VEC_PUSH(&final_args, &(char*) { NULL });

    subprocess_run(final_args.buf);
}

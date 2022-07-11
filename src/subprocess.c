/*
 * SUBPROCESS -- Subprocess execution facilities.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#define _GNU_SOURCE
#include "subprocess.h"

#include <assert.h>
#include <errno.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>

#include <a3/str.h>

static char* cstring_clone(A3CString s) {
    assert(s.ptr);

    A3String ret = a3_string_alloc(s.len + 1);
    a3_string_concat(ret, 2, s, A3_CS("\0"));

    return (char*)ret.ptr;
}

static void subprocess_run(char* const argv[]) {
    assert(argv);

    pid_t pid;
    int   res = posix_spawnp(&pid, argv[0], NULL, NULL, argv, NULL);
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

void assemble(A3CString src, A3CString dst) {
    assert(src.ptr);
    assert(dst.ptr);

    subprocess_run(
        (char* const[]) { "nasm", "-felf64", "-o", cstring_clone(dst), cstring_clone(src), NULL });
}

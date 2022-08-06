/*
 * CONFIG -- Global configuration.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <stdbool.h>
#include <stdio.h>

#include <a3/str.h>
#include <a3/vec.h>

typedef enum FileType {
    FILE_SRC,
    FILE_OBJ,
    FILE_ASM,
} FileType;

typedef struct File {
    A3CString path;
    A3CString name;
    A3CString out;
    FileType  type;
} File;

typedef struct Config {
    A3Vec     files;
    A3CString out_path;
    A3CString tmp_dir;
    A3Vec     preprocess_args;
    bool      dump_ast;
    bool      keep_tmp;
    bool      output_preprocessed;
    bool      output_asm;
    bool      output_obj;
} Config;

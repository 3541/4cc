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

typedef struct Config {
    A3CString src_path;
    A3CString src_name;
    A3CString out_path;
    A3CString tmp_dir;
    A3CString preprocess_out_path;
    A3Vec     preprocess_args;
    A3CString asm_out_path;
    A3CString obj_out_path;
    FILE*     asm_out;
    bool      dump_ast;
    bool      keep_tmp;
    bool      output_preprocessed;
    bool      output_asm;
    bool      output_obj;
} Config;

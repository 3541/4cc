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

typedef struct Config {
    A3CString src_path;
    A3CString src_name;
    A3CString src;
    A3CString out_path;
    A3CString tmp_dir;
    A3CString asm_out_path;
    A3CString obj_out_path;
    FILE*     asm_out;
    bool      dump_ast;
    bool      keep_tmp;
    bool      output_asm;
    bool      output_obj;
} Config;

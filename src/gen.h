/*
 * GEN -- x86_64 Codegen.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 *
 * Emits nasm-style assembly.
 */

#pragma once

#include <stdbool.h>
#include <stdio.h>

#include <a3/str.h>

// ast.h
typedef struct Vertex Vertex;

// config.h
typedef struct Config Config;

#define GEN_OK  true
#define GEN_ERR false
bool gen(Config const*, A3CString src, Vertex*);

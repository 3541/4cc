/*
 * PARSE -- Parser.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <a3/str.h>

#define PARSE_ERRORS_MAX 512

// ast.h
typedef struct Vertex Vertex;
// lex.h
typedef struct Lexer Lexer;
// parse.c
typedef struct Parser Parser;

Parser* parse_new(A3CString src, Lexer* lexer);
Vertex* parse(Parser*);

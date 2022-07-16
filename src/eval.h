/*
 * EVAL -- Compile-time expression evaluation.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <stdbool.h>
#include <stdint.h>

#include <a3/str.h>

// ast.h
typedef struct Expr Expr;

typedef struct EvalResult {
    bool    ok;
    int64_t value;
} EvalResult;

EvalResult eval(A3CString src, Expr const*);

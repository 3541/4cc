/*
 * SUBPROCESS -- Subprocess execution facilities.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <a3/str.h>
#include <a3/vec.h>

void preprocess(A3CString src, A3CString dst, A3Vec* args);
void assemble(A3CString src, A3CString dst);
void link(A3CString src, A3CString dst);

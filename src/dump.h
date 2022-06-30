/*
 * DUMP -- AST printing.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <stdbool.h>

// ast.h
typedef struct Vertex Vertex;

bool dump(Vertex*);

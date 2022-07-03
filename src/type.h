/*
 * TYPE -- Types.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <stdbool.h>

#include <a3/sll.h>
#include <a3/str.h>

// ast.h
typedef struct Vertex Vertex;
typedef struct Item   Item;

typedef struct Type     Type;
typedef struct Registry Registry;
typedef struct Scope    Scope;

typedef enum TypeType { TY_INT, TY_CHAR, TY_ARRAY, TY_FN, TY_PTR } TypeType;

typedef struct Type {
    TypeType type;

    union {
        struct {
            Type const* parent;
            size_t      len;
        };
        struct {
            A3_SLL(, Item) params;
            Type const* ret;
        };
    };
} Type;

typedef struct Obj {
    A3CString   name;
    Type const* type;
    bool        global;
    union {
        size_t stack_offset;
        size_t stack_depth; // TY_FN.
    };
} Obj;

extern Type const* BUILTIN_TYPES[2];

Registry* type_registry_new(void);
A3String  type_name(Type const*);
bool      type_is_scalar(Type const*);
size_t    type_size(Type const*);
bool      type(Registry*, A3CString src, Vertex*);

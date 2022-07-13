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
typedef struct Member Member;
typedef struct Expr   Expr;

typedef struct Type     Type;
typedef struct Registry Registry;
typedef struct Scope    Scope;

typedef enum TypeType {
    TY_VOID,
    TY_I8,
    TY_I16,
    TY_I32,
    TY_I64,
    TY_USIZE,
    TY_CHAR,

    TY_ARRAY,
    TY_FN,
    TY_PTR,
    TY_STRUCT,
    TY_UNION,
} TypeType;

typedef struct Param Param;
typedef struct Param {
    A3_SLL_LINK(Param) link;
    Type const* type;
} Param;

typedef struct Type {
    TypeType type;
    size_t   size;
    size_t   align;

    union {
        // TY_STRUCT
        struct {
            A3CString name;
            A3_SLL(, Member) members;
        };

        // TY_ARRAY and TY_PTR.
        struct {
            Type const* parent;
            size_t      len;
        };

        // TY_FN
        struct {
            A3_SLL(, Param) params;
            Type const* ret;
        };
    };
} Type;

typedef struct Obj {
    A3CString   name;
    Type const* type;
    bool        global;
    bool        defined;

    union {
        struct {
            Expr*  init;
            size_t stack_offset;
        };

        // TY_FN.
        struct {
            size_t stack_depth;
            Scope* scope;
            A3_SLL(, Item) params;
        };
    };
} Obj;

Registry*     type_registry_new(void);
A3String      type_name(Type const*);
bool          type_is_scalar(Type const*);
Member const* type_struct_find_member(Type const*, A3CString name);
bool          type(Registry*, A3CString src, Vertex*);

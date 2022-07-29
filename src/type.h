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
typedef struct Init   Init;

typedef struct Type     Type;
typedef struct Registry Registry;
typedef struct Scope    Scope;

typedef enum TypeType {
    TY_VOID,
    TY_I8,
    TY_I16,
    TY_I32,
    TY_I64,
    TY_ISIZE = TY_I64,
    TY_U8,
    TY_U16,
    TY_U32,
    TY_U64,
    TY_USIZE = TY_U64,

    TY_ARRAY,
    TY_ENUM,
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

#define TYPE_ARRAY_UNSIZED SIZE_MAX

typedef struct Type {
    TypeType type;
    size_t   size;
    size_t   align;

    union {
        bool is_signed; // Builtins

        // TY_STRUCT and TY_UNION.
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
            bool        is_variadic;
        };
    };
} Type;

typedef struct Obj {
    A3CString   name;
    Type const* type;
    bool        is_global;
    bool        is_static;
    bool        is_defined;
    bool        is_named_literal;

    union {
        struct {
            Init*  init;
            size_t stack_offset;
        };

        // TY_FN.
        struct {
            size_t stack_depth;
            Scope* scope;
            A3_SLL(, Item) params;
        };

        uint32_t value; // TY_ENUM_CONSTANT
    };
} Obj;

extern Type const* BUILTIN_TYPES[];

Registry*     type_registry_new(void);
A3String      type_name(Type const*);
bool          type_is_scalar(Type const*);
bool          type_is_scalar_value(Type const*);
Member const* type_struct_find_member(Type const*, A3CString name);
TypeType      type_to_underlying(TypeType);
bool          type(Registry*, A3CString src, Vertex*);

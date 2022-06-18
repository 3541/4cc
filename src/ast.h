/*
 * AST -- Syntax tree.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <stdint.h>

#include <a3/str.h>

typedef enum VertexType {
    V_BIN_OP,
    V_LIT,
} VertexType;

typedef enum BinOpType { OP_ADD, OP_SUB, OP_MUL, OP_DIV } BinOpType;

typedef enum LiteralType { LIT_NUM } LiteralType;

typedef struct Vertex Vertex;

typedef struct Vertex {
    A3CString  span;
    VertexType type;

    union {
        struct {
            BinOpType bin_op_type;
            Vertex*   lhs;
            Vertex*   rhs;
        };
        struct {
            LiteralType lit_type;
            union {
                int64_t lit_num;
            };
        };
    };
} Vertex;

typedef struct AstVisitor AstVisitor;

typedef void (*AstVisitorCallback)(AstVisitor*, Vertex*);

typedef struct AstVisitor {
    void*              ctx;
    AstVisitorCallback visit_lit;
    AstVisitorCallback visit_bin_op;
} AstVisitor;

Vertex* vertex_bin_op_new(A3CString span, BinOpType type, Vertex* lhs, Vertex* rhs);
Vertex* vertex_lit_num_new(A3CString span, int64_t);
void    vertex_visit(AstVisitor*, Vertex*);

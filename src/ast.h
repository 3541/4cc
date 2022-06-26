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

#include <a3/sll.h>
#include <a3/str.h>

typedef enum VertexType {
    V_BIN_OP,
    V_UNARY_OP,
    V_LIT,
    V_STMT,
} VertexType;

typedef enum BinOpType {
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_EQ,
    OP_NE,
    OP_LT,
    OP_LE,
    OP_GT,
    OP_GE
} BinOpType;

typedef enum UnaryOpType { OP_UNARY_ADD, OP_NEG } UnaryOpType;
typedef enum StmtType { STMT_EXPR_STMT } StmtType;
typedef enum LiteralType { LIT_NUM } LiteralType;

typedef struct Vertex Vertex;

typedef struct Vertex {
    A3CString  span;
    VertexType type;

    union {
        // V_BIN_OP
        struct {
            BinOpType bin_op_type;
            Vertex*   lhs;
            Vertex*   rhs;
        };

        // V_UNARY_OP
        struct {
            UnaryOpType unary_op_type;
            Vertex*     operand;
        };

        // V_LIT
        struct {
            LiteralType lit_type;

            union {
                int64_t lit_num; // LIT_NUM
            };
        };

        // V_STMT
        struct {
            StmtType stmt_type;
            A3_SLL_LINK(Vertex) link;

            union {
                Vertex* expr; // STMT_EXPR_STMT
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
    AstVisitorCallback visit_unary_op;
    AstVisitorCallback visit_expr_stmt;
} AstVisitor;

Vertex* vertex_bin_op_new(A3CString span, BinOpType, Vertex* lhs, Vertex* rhs);
Vertex* vertex_unary_op_new(A3CString span, UnaryOpType, Vertex* operand);
Vertex* vertex_lit_num_new(A3CString span, int64_t);
Vertex* vertex_expr_stmt_new(A3CString span, Vertex* expr);
void    vertex_visit(AstVisitor*, Vertex*);

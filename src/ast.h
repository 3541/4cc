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

#include <a3/ht.h>
#include <a3/sll.h>
#include <a3/str.h>

typedef struct Vertex Vertex;

typedef enum VertexType {
    V_BIN_OP,
    V_FN,
    V_LIT,
    V_STMT,
    V_UNARY_OP,
    V_VAR,
} VertexType;

typedef enum BinOpType {
    OP_ADD,
    OP_ASSIGN,
    OP_DIV,
    OP_EQ,
    OP_GE,
    OP_GT,
    OP_LE,
    OP_LT,
    OP_MUL,
    OP_NE,
    OP_SUB,
} BinOpType;

typedef enum UnaryOpType { OP_UNARY_ADD, OP_NEG } UnaryOpType;
typedef enum StmtType { STMT_EXPR_STMT } StmtType;
typedef enum LiteralType { LIT_NUM } LiteralType;

typedef struct Var {
    A3CString name;
    size_t    stack_offset;
} Var;
A3_HT_DEFINE_STRUCTS(A3CString, Var)

typedef struct Fn {
    A3CString name;
    A3_SLL(body, Vertex) body;
    A3_HT(A3CString, Var) scope;
    size_t stack_depth;
} Fn;

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

        Var* var; // V_VAR
        Fn   fn;  // V_FN
    };
} Vertex;

typedef struct AstVisitor AstVisitor;

typedef bool (*AstVisitorCallback)(AstVisitor*, Vertex*);

typedef struct AstVisitor {
    void*              ctx;
    AstVisitorCallback visit_lit;
    AstVisitorCallback visit_bin_op;
    AstVisitorCallback visit_unary_op;
    AstVisitorCallback visit_expr_stmt;
    AstVisitorCallback visit_var;
    AstVisitorCallback visit_fn;
} AstVisitor;

Vertex* vertex_bin_op_new(A3CString span, BinOpType, Vertex* lhs, Vertex* rhs);
Vertex* vertex_unary_op_new(A3CString span, UnaryOpType, Vertex* operand);
Vertex* vertex_lit_num_new(A3CString span, int64_t);
Vertex* vertex_expr_stmt_new(A3CString span, Vertex* expr);
Vertex* vertex_fn_new(A3CString name);
Vertex* vertex_var_new(A3CString span, Fn* scope);
bool    vertex_visit(AstVisitor*, Vertex*);

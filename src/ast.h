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
#include <a3/util.h>

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
typedef enum StmtType { STMT_EXPR_STMT, STMT_RET, STMT_BLOCK, STMT_IF, STMT_EMPTY } StmtType;
typedef enum LiteralType { LIT_NUM } LiteralType;

typedef struct Var {
    A3CString name;
    size_t    stack_offset;
} Var;
A3_HT_DEFINE_STRUCTS(A3CString, Var)

typedef struct Scope Scope;
typedef struct Scope {
    Scope* parent;
    A3_HT(A3CString, Var) scope;
    size_t stack_depth;
} Scope;

typedef struct Block {
    A3_SLL(body, Vertex) body;
    Scope* scope;
} Block;

typedef struct Fn {
    A3CString name;
    Vertex*   body;
} Fn;

typedef struct BinOp {
    BinOpType type;
    Vertex*   lhs;
    Vertex*   rhs;
} BinOp;

typedef struct UnaryOp {
    UnaryOpType type;
    Vertex*     operand;
} UnaryOp;

typedef struct Literal {
    LiteralType type;

    union {
        int64_t num; // LIT_NUM
    };
} Literal;

typedef struct Statement Statement;
typedef struct Statement {
    StmtType type;
    A3_SLL_LINK(Vertex) link;

    union {
        Vertex* expr;  // STMT_EXPR_STMT and STMT_RET.
        Block   block; // STMT_BLOCK
        // STMT_IF
        struct {
            Vertex*    cond;
            Statement* body_true;
            Statement* body_false;
        };
    };
} Statement;

typedef struct Vertex {
    A3CString  span;
    VertexType type;

    union {
        BinOp     bin_op;
        UnaryOp   unary_op;
        Literal   lit;
        Statement stmt;
        Var*      var;
        Fn        fn;
    };
} Vertex;

#define VERTEX(P, F) A3_CONTAINER_OF(P, Vertex, F)

typedef struct AstVisitor AstVisitor;

typedef bool (*AstVisitorCallback)(AstVisitor*, Vertex*);

typedef struct AstVisitor {
    void* ctx;
    bool (*visit_lit)(AstVisitor*, Literal*);
    bool (*visit_bin_op)(AstVisitor*, BinOp*);
    bool (*visit_unary_op)(AstVisitor*, UnaryOp*);
    bool (*visit_expr_stmt)(AstVisitor*, Statement*);
    bool (*visit_ret)(AstVisitor*, Statement*);
    bool (*visit_if_stmt)(AstVisitor*, Statement*);
    bool (*visit_var)(AstVisitor*, Var*);
    bool (*visit_fn)(AstVisitor*, Fn*);
    bool (*visit_block)(AstVisitor*, Block*);
} AstVisitor;

Scope* scope_new(Scope* parent);

Vertex* vertex_bin_op_new(A3CString span, BinOpType, Vertex* lhs, Vertex* rhs);
Vertex* vertex_unary_op_new(A3CString span, UnaryOpType, Vertex* operand);
Vertex* vertex_lit_num_new(A3CString span, int64_t);
Vertex* vertex_expr_stmt_new(A3CString span, Vertex* expr);
Vertex* vertex_ret_new(A3CString span, Vertex* expr);
Vertex* vertex_empty_new(A3CString span);
Vertex* vertex_block_new(Scope*);
Vertex* vertex_fn_new(A3CString name, Vertex* body);
Vertex* vertex_var_new(A3CString span, Scope* scope);
Vertex* vertex_if_new(A3CString span, Vertex* cond, Statement* body_true, Statement* body_false);
bool    vertex_visit(AstVisitor*, Vertex*);

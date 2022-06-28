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

typedef struct Expr      Expr;
typedef struct Scope     Scope;
typedef struct Statement Statement;
typedef struct Vertex    Vertex;

typedef enum VertexType {
    V_EXPR,
    V_FN,
    V_STMT,
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

typedef enum StmtType {
    STMT_BLOCK,
    STMT_EMPTY,
    STMT_EXPR_STMT,
    STMT_IF,
    STMT_LOOP,
    STMT_RET,
} StmtType;

typedef enum UnaryOpType { OP_UNARY_ADD, OP_NEG, OP_ADDR, OP_DEREF } UnaryOpType;
typedef enum ExprType { EXPR_BIN_OP, EXPR_UNARY_OP, EXPR_LIT, EXPR_VAR } ExprType;
typedef enum LiteralType { LIT_NUM } LiteralType;

typedef struct Var {
    A3CString name;
    size_t    stack_offset;
} Var;
A3_HT_DEFINE_STRUCTS(A3CString, Var)

typedef struct Scope {
    Scope* parent;
    A3_HT(A3CString, Var) scope;
    size_t stack_depth;
} Scope;

typedef struct Block {
    A3_SLL(body, Statement) body;
    Scope* scope;
} Block;

typedef struct Fn {
    A3CString name;
    Block*    body;
} Fn;

typedef struct BinOp {
    BinOpType type;
    Expr*     lhs;
    Expr*     rhs;
} BinOp;

typedef struct UnaryOp {
    UnaryOpType type;
    Expr*       operand;
} UnaryOp;

typedef struct Literal {
    LiteralType type;

    union {
        int64_t num; // LIT_NUM
    };
} Literal;

typedef struct Expr {
    ExprType type;

    union {
        BinOp   bin_op;
        UnaryOp unary_op;
        Literal lit;
        Var*    var;
    };
} Expr;

typedef struct If {
    Expr*      cond;
    Statement* body_true;
    Statement* body_false;
} If;

typedef struct Loop {
    Statement* init;
    Expr*      cond;
    Expr*      post;
    Statement* body;
} Loop;

typedef struct Statement {
    StmtType type;
    A3_SLL_LINK(Statement) link;

    union {
        Expr* expr;    // STMT_EXPR_STMT and STMT_RET.
        Block block;   // STMT_BLOCK
        If    if_stmt; // STMT_IF
        Loop  loop;    // STMT_LOOP
    };
} Statement;

typedef struct Vertex {
    A3CString  span;
    VertexType type;

    union {
        Expr      expr;
        Statement stmt;
        Fn        fn;
    };
} Vertex;

#define VERTEX(P, F) A3_CONTAINER_OF(P, Vertex, F)
#define STMT(P, F)   A3_CONTAINER_OF(P, Statement, F)
#define EXPR(P, F)   A3_CONTAINER_OF(P, Expr, F)
#define SPAN(P, F)   (A3_CONTAINER_OF(P, Vertex, F)->span)

typedef struct AstVisitor AstVisitor;

typedef bool (*AstVisitorCallback)(AstVisitor*, Vertex*);

typedef struct AstVisitor {
    void* ctx;
    bool (*visit_bin_op)(AstVisitor*, BinOp*);
    bool (*visit_unary_op)(AstVisitor*, UnaryOp*);
    bool (*visit_lit)(AstVisitor*, Literal*);
    bool (*visit_var)(AstVisitor*, Var**);
    bool (*visit_expr_stmt)(AstVisitor*, Statement*);
    bool (*visit_ret)(AstVisitor*, Statement*);
    bool (*visit_if_stmt)(AstVisitor*, If*);
    bool (*visit_block)(AstVisitor*, Block*);
    bool (*visit_fn)(AstVisitor*, Fn*);
    bool (*visit_loop)(AstVisitor*, Loop*);
} AstVisitor;

Scope* scope_new(Scope* parent);

Expr*      vertex_bin_op_new(A3CString span, BinOpType, Expr* lhs, Expr* rhs);
Expr*      vertex_unary_op_new(A3CString span, UnaryOpType, Expr* operand);
Expr*      vertex_lit_num_new(A3CString span, int64_t);
Expr*      vertex_var_new(A3CString span, Scope* scope);
Statement* vertex_expr_stmt_new(A3CString span, Expr* expr);
Statement* vertex_ret_new(A3CString span, Expr* expr);
Statement* vertex_empty_new(A3CString span);
If*        vertex_if_new(A3CString span, Expr* cond, Statement* body_true, Statement* body_false);
Block*     vertex_block_new(Scope*);
Fn*        vertex_fn_new(A3CString name, Block* body);
Loop* vertex_loop_new(A3CString span, Statement* init, Expr* cond, Expr* post, Statement* body);
bool  vertex_visit(AstVisitor*, Vertex*);

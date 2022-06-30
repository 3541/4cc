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
#include <a3/util.h>

#include "lex.h"

// type.h
typedef struct Type Type;

// res.h
typedef struct Obj Obj;

typedef struct Expr   Expr;
typedef struct Scope  Scope;
typedef struct Item   Item;
typedef struct Vertex Vertex;

typedef enum VertexType {
    V_DECL,
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

typedef struct Block {
    A3_SLL(body, Item) body;
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

typedef struct Var {
    A3CString name;
    Obj*      obj;
} Var;

typedef struct Expr {
    ExprType    type;
    Type const* res_type;

    union {
        BinOp   bin_op;
        UnaryOp unary_op;
        Literal lit;
        Var     var;
    };
} Expr;

typedef struct If {
    Expr* cond;
    Item* body_true;
    Item* body_false;
} If;

typedef struct Loop {
    Item* init;
    Expr* cond;
    Expr* post;
    Item* body;
} Loop;

typedef enum PTypeType { PTY_PTR, PTY_BASE, PTY_BUILTIN } PTypeType;

typedef struct PType PType;
typedef struct PType {
    PTypeType type;

    union {
        TokenType builtin;
        A3CString name;
        PType*    parent;
    };
} PType;

typedef struct Item {
    A3_SLL_LINK(Item) link;

    union {
        // V_STMT
        struct {
            StmtType type;
            union {
                Expr* expr;    // STMT_EXPR_STMT and STMT_RET.
                Block block;   // STMT_BLOCK
                If    if_stmt; // STMT_IF
                Loop  loop;    // STMT_LOOP
            };
        };

        // V_DECL
        struct {
            A3CString name;
            union {
                PType*      decl_ptype;
                Type const* decl_type;
            };
        };
    };
} Item;

typedef struct Vertex {
    A3CString  span;
    VertexType type;

    union {
        Expr expr;
        Item item;
        Fn   fn;
    };
} Vertex;

#define VERTEX(P, F) A3_CONTAINER_OF(P, Vertex, F)
#define ITEM(P, F)   A3_CONTAINER_OF(P, Item, F)
#define EXPR(P, F)   A3_CONTAINER_OF(P, Expr, F)
#define SPAN(P, F)   (A3_CONTAINER_OF(P, Vertex, F)->span)

typedef struct AstVisitor AstVisitor;

typedef bool (*AstVisitorCallback)(AstVisitor*, Vertex*);

typedef struct AstVisitor {
    void* ctx;
    bool (*visit_bin_op)(AstVisitor*, BinOp*);
    bool (*visit_unary_op)(AstVisitor*, UnaryOp*);
    bool (*visit_lit)(AstVisitor*, Literal*);
    bool (*visit_var)(AstVisitor*, Var*);
    bool (*visit_expr_stmt)(AstVisitor*, Item*);
    bool (*visit_ret)(AstVisitor*, Item*);
    bool (*visit_decl)(AstVisitor*, Item*);
    bool (*visit_if)(AstVisitor*, If*);
    bool (*visit_block)(AstVisitor*, Block*);
    bool (*visit_loop)(AstVisitor*, Loop*);
    bool (*visit_fn)(AstVisitor*, Fn*);
} AstVisitor;

Expr*  vertex_bin_op_new(A3CString span, BinOpType, Expr* lhs, Expr* rhs);
Expr*  vertex_unary_op_new(A3CString span, UnaryOpType, Expr* operand);
Expr*  vertex_lit_num_new(A3CString span, int64_t);
Expr*  vertex_var_new(A3CString span);
Item*  vertex_expr_stmt_new(A3CString span, Expr* expr);
Item*  vertex_ret_new(A3CString span, Expr* expr);
Item*  vertex_empty_new(A3CString span);
Item*  vertex_decl_new(A3CString span, A3CString name, PType*);
If*    vertex_if_new(A3CString span, Expr* cond, Item* body_true, Item* body_false);
Block* vertex_block_new(void);
Fn*    vertex_fn_new(A3CString name, Block* body);
Loop*  vertex_loop_new(A3CString span, Item* init, Expr* cond, Expr* post, Item* body);
bool   vertex_visit(AstVisitor*, Vertex*);

PType* ptype_builtin_new(TokenType);
PType* ptype_ptr_to(PType*);

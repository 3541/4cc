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

#include "error.h"
#include "lex.h"

// type.h
typedef struct Type Type;
typedef struct Obj  Obj;

typedef struct Expr   Expr;
typedef struct Scope  Scope;
typedef struct Item   Item;
typedef struct Vertex Vertex;
typedef struct PType  PType;
typedef struct Member Member;

typedef enum VertexType {
    V_DECL,
    V_EXPR,
    V_STMT,
    V_UNIT,
} VertexType;

typedef enum BinOpType {
    OP_ADD,
    OP_AND,
    OP_ASSIGN,
    OP_DIV,
    OP_EQ,
    OP_GE,
    OP_GT,
    OP_LE,
    OP_LT,
    OP_MUL,
    OP_NE,
    OP_OR,
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

typedef enum UnaryOpType {
    OP_ADDR,
    OP_BW_NOT,
    OP_DEREF,
    OP_NEG,
    OP_NOT,
    OP_SIZEOF,
    OP_UNARY_ADD,
} UnaryOpType;

typedef enum ExprType {
    EXPR_BIN_OP,
    EXPR_CALL,
    EXPR_LIT,
    EXPR_MEMBER,
    EXPR_UNARY_OP,
    EXPR_VAR,
} ExprType;

typedef enum LiteralType { LIT_NUM } LiteralType;

typedef struct Block {
    A3_SLL(body, Item) body;
    Scope* scope;
} Block;

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

    Obj* obj;
} Var;

typedef struct Arg Arg;
typedef struct Arg {
    Expr* expr;
    A3_SLL_LINK(Arg) link;
} Arg;

typedef struct Call {
    A3CString name;
    A3_SLL(args, Arg) args;
    Obj* obj;
} Call;

typedef struct MemberAccess {
    Expr* lhs;
    union {
        A3CString     name;
        Member const* rhs;
    };
} MemberAccess;

typedef struct Expr {
    ExprType    type;
    Type const* res_type;

    union {
        BinOp        bin_op;
        UnaryOp      unary_op;
        Literal      lit;
        Var          var;
        Call         call;
        MemberAccess member;
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

typedef struct Member {
    A3CString name;
    A3_SLL_LINK(Member) link;
    size_t offset;

    union {
        PType*      ptype;
        Type const* type;
    };
} Member;

typedef enum PTypeType { PTY_PTR, PTY_BUILTIN, PTY_FN, PTY_ARRAY, PTY_STRUCT, PTY_UNION } PTypeType;

typedef struct PType {
    PTypeType type;
    Span      span;

    union {
        TokenType builtin; // PTY_BUILTIN

        // PTY_STRUCT
        struct {
            Span name;
            A3_SLL(, Member) members;
        };

        // PTY_FN
        struct {
            A3_SLL(, Item) params;
            PType* ret;
        };

        // PTY_ARRAY and PTY_PTR.
        struct {
            PType* parent;
            size_t len; // PTY_ARRAY
        };
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
                PType* decl_ptype;
                Obj*   obj;
            };
            union {
                Block*    body; // TY_FN.
                A3CString lit_str;
            };
        };
    };
} Item;

typedef struct Unit {
    A3_SLL(items, Item) items;
} Unit;

typedef struct Vertex {
    VertexType type;
    Span       span;

    union {
        Expr expr;
        Item item;
        Unit unit;
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
    bool (*pre)(AstVisitor*, Vertex*);
    bool (*visit_bin_op)(AstVisitor*, BinOp*);
    bool (*visit_unary_op)(AstVisitor*, UnaryOp*);
    bool (*visit_lit)(AstVisitor*, Literal*);
    bool (*visit_var)(AstVisitor*, Var*);
    bool (*visit_call)(AstVisitor*, Call*);
    bool (*visit_member)(AstVisitor*, MemberAccess*);
    bool (*visit_expr_stmt)(AstVisitor*, Item*);
    bool (*visit_ret)(AstVisitor*, Item*);
    bool (*visit_decl)(AstVisitor*, Item*);
    bool (*visit_if)(AstVisitor*, If*);
    bool (*visit_block)(AstVisitor*, Block*);
    bool (*visit_loop)(AstVisitor*, Loop*);
} AstVisitor;

Expr*  vertex_bin_op_new(Span, BinOpType, Expr* lhs, Expr* rhs);
Expr*  vertex_unary_op_new(Span, UnaryOpType, Expr* operand);
Expr*  vertex_lit_num_new(Span, int64_t);
Expr*  vertex_var_new(Span, A3CString name);
Expr*  vertex_call_new(Span, A3CString name);
Expr*  vertex_member_new(Span, Expr* lhs, A3CString rhs_name);
Item*  vertex_expr_stmt_new(Span, Expr* expr);
Item*  vertex_ret_new(Span, Expr* expr);
Item*  vertex_empty_new(Span);
Item*  vertex_decl_new(Span, A3CString name, PType*);
If*    vertex_if_new(Span, Expr* cond, Item* body_true, Item* body_false);
Block* vertex_block_new(void);
Loop*  vertex_loop_new(Span, Item* init, Expr* cond, Expr* post, Item* body);
Unit*  vertex_unit_new(void);
bool   vertex_visit(AstVisitor*, Vertex*);

PType* ptype_builtin_new(Span, TokenType);
PType* ptype_ptr_new(Span, PType*);
PType* ptype_fn_new(Span, PType* ret_type);
PType* ptype_array_new(Span, PType*, size_t);
PType* ptype_aggregate_new(Span, PTypeType, Span name);

Arg*    arg_new(Expr*);
Member* member_new(A3CString name, PType*);

/*
 * AST -- Syntax tree.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <stdbool.h>
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
    V_INIT,
    V_STMT,
    V_UNIT,
} VertexType;

typedef enum BinOpType {
    OP_ADD,
    OP_AND,
    OP_ASSIGN,
    OP_BW_AND,
    OP_BW_OR,
    OP_BW_XOR,
    OP_CAST,
    OP_DIV,
    OP_EQ,
    OP_GE,
    OP_GT,
    OP_LE,
    OP_LT,
    OP_MOD,
    OP_MUL,
    OP_NE,
    OP_OR,
    OP_SHL,
    OP_SHR,
    OP_SUB,
} BinOpType;

typedef enum StmtType {
    STMT_BLOCK,
    STMT_BREAK,
    STMT_CONTINUE,
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
    EXPR_COND,
    EXPR_LIT,
    EXPR_MEMBER,
    EXPR_TYPE,
    EXPR_UNARY_OP,
    EXPR_VAR,
} ExprType;

typedef enum LiteralType { LIT_NUM, LIT_STR } LiteralType;

typedef A3_SLL(Items, Item) Items;

typedef struct Block {
    Items  body;
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
    Obj*        storage; // LIT_STR

    union {
        uintmax_t num; // LIT_NUM
        A3CString str; // LIT_STR - pre-type.
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
    Expr*             callee;
    A3_SLL(args, Arg) args;
} Call;

typedef struct MemberAccess {
    Expr* lhs;
    union {
        A3CString     name;
        Member const* rhs;
    };
} MemberAccess;

typedef struct CondExpr {
    Expr* cond;
    Expr* res_true;
    Expr* res_false;
} CondExpr;

typedef struct Expr {
    ExprType type;
    union {
        PType*      res_ptype; // EXPR_TYPE
        Type const* res_type;
    };

    union {
        BinOp        bin_op;
        UnaryOp      unary_op;
        Literal      lit;
        Var          var;
        Call         call;
        MemberAccess member;
        CondExpr     cond;
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
    bool  cond_first;
} Loop;

typedef struct Member {
    A3CString name;
    A3_SLL_LINK(Member) link;
    size_t offset;

    union {
        Expr* init; // PTY_ENUM

        // PTY_STRUCT and PTY_UNION.
        PType*      ptype;
        Type const* type;
    };
} Member;

typedef enum PTypeType {
    PTY_ARRAY,
    PTY_BUILTIN,
    PTY_DEFINED,
    PTY_DUMMY,
    PTY_ENUM,
    PTY_FN,
    PTY_PTR,
    PTY_STRUCT,
    PTY_UNION,
} PTypeType;

typedef enum PTypeBuiltin {
    PTY_NOTHING   = 0,
    PTY_VOID      = 1,
    PTY_CHAR      = 1 << 1,
    PTY_SHORT     = 1 << 2,
    PTY_INT       = 1 << 3,
    PTY_LONG      = 1 << 4,
    PTY_LONG_LONG = 1 << 5,

    PTY_I8    = 1 << 6,
    PTY_I16   = 1 << 7,
    PTY_I32   = 1 << 8,
    PTY_I64   = 1 << 9,
    PTY_ISIZE = 1 << 10,

    PTY_U8    = 1 << 11,
    PTY_U16   = 1 << 12,
    PTY_U32   = 1 << 13,
    PTY_U64   = 1 << 14,
    PTY_USIZE = 1 << 15,

    PTY_SIGNED   = 1 << 16,
    PTY_UNSIGNED = 1 << 17,

    PTY_TYPE_QUALIFIERS = PTY_SHORT | PTY_LONG | PTY_LONG_LONG,
    PTY_TYPES = PTY_I8 | PTY_I16 | PTY_I32 | PTY_I64 | PTY_ISIZE | PTY_U8 | PTY_U16 | PTY_U32 |
                PTY_U64 | PTY_USIZE | PTY_INT | PTY_CHAR,
} PTypeBuiltin;

typedef int PTypeBuiltinType;

typedef struct DeclAttributes {
    bool is_typedef;
    bool is_extern;
    bool is_variadic;
    bool is_static;
} DeclAttributes;

typedef struct PType {
    PTypeType      type;
    Span           span;
    DeclAttributes attributes;

    union {
        // PTY_BUILTIN and PTY_DEFINED.
        struct {
            PTypeBuiltinType builtin_type;
            A3CString        defined_name; // PTY_DEFINED
        };

        // PTY_STRUCT and PTY_UNION.
        struct {
            Span name;
            A3_SLL(, Member) members;
        };

        // PTY_FN
        struct {
            PType* ret;
            A3_SLL(, Item) params;
        };

        // PTY_ARRAY and PTY_PTR.
        struct {
            PType* parent;
            Expr*  len; // PTY_ARRAY
        };
    };
} PType;

typedef enum InitType { INIT_EXPR, INIT_LIST } InitType;

typedef struct Init Init;
typedef struct Init {
    InitType type;
    A3_SLL_LINK(Init) link;

    union {
        Expr* expr;
        A3_SLL(, Init) list;
    };
} Init;

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
            A3CString      name;
            Obj*           obj;
            DeclAttributes attributes;

            union {
                PType*      decl_ptype;
                Type const* decl_type;
            };

            union {
                Block* body; // TY_FN.
                Init*  init;
            };
        };
    };
} Item;

typedef struct Unit {
    Items items;
} Unit;

typedef struct Vertex {
    VertexType type;
    Span       span;

    union {
        Expr expr;
        Item item;
        Unit unit;
        Init init;
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
    bool (*visit_expr_cond)(AstVisitor*, CondExpr*);
    bool (*visit_expr_type)(AstVisitor*, Expr*);
    bool (*visit_expr_stmt)(AstVisitor*, Item*);
    bool (*visit_ret)(AstVisitor*, Item*);
    bool (*visit_break_continue)(AstVisitor*, Item*);
    bool (*visit_decl)(AstVisitor*, Item*);
    bool (*visit_if)(AstVisitor*, If*);
    bool (*visit_block)(AstVisitor*, Block*);
    bool (*visit_loop)(AstVisitor*, Loop*);
    bool (*visit_init)(AstVisitor*, Init*);
} AstVisitor;

#define LOOP_COND_PRE  true
#define LOOP_COND_POST false

Expr*  vertex_bin_op_new(Span, BinOpType, Expr* lhs, Expr* rhs);
Expr*  vertex_unary_op_new(Span, UnaryOpType, Expr* operand);
Expr*  vertex_lit_num_new(Span, Type const*, uintmax_t);
Expr*  vertex_lit_str_new(Span, A3CString);
Expr*  vertex_var_new(Span, A3CString name);
Expr*  vertex_call_new(Span, Expr* callee);
Expr*  vertex_member_new(Span, Expr* lhs, A3CString rhs_name);
Expr*  vertex_expr_cond_new(Span, Expr* cond, Expr* res_true, Expr* res_false);
Expr*  vertex_expr_type_new(Span, PType*);
Item*  vertex_expr_stmt_new(Span, Expr* expr);
Item*  vertex_ret_new(Span, Expr* expr);
Item*  vertex_empty_new(Span);
Item*  vertex_break_continue_new(Span, StmtType);
Item*  vertex_decl_new(Span, A3CString name, PType*);
If*    vertex_if_new(Span, Expr* cond, Item* body_true, Item* body_false);
Block* vertex_block_new(void);
Loop*  vertex_loop_new(Span, bool cond_pos, Item* init, Expr* cond, Expr* post, Item* body);
Unit*  vertex_unit_new(void);
Init*  vertex_init_expr_new(Span, Expr*);
Init*  vertex_init_list_new(void);
void   vertex_init_lit_str_to_list(Init*);
bool   vertex_visit(AstVisitor*, Vertex*);

PType* ptype_builtin_new(Span, PTypeBuiltinType);
PType* ptype_ptr_new(Span, PType*);
PType* ptype_fn_new(Span, PType* ret_type);
PType* ptype_array_new(Span, PType*, Expr* len);
PType* ptype_aggregate_new(Span, PTypeType, Span name);
PType* ptype_defined_new(Span name);

Arg*    arg_new(Expr*);
Member* member_new(A3CString name, PType*);

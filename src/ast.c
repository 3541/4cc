/*
 * AST -- Syntax tree.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include "ast.h"

#include <assert.h>
#include <stdint.h>

#include <a3/ht.h>
#include <a3/sll.h>
#include <a3/str.h>
#include <a3/util.h>

Expr* vertex_bin_op_new(A3CString span, BinOpType type, Expr* lhs, Expr* rhs) {
    assert(lhs);
    assert(rhs);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_EXPR,
                      .expr = { .type     = EXPR_BIN_OP,
                                .res_type = NULL,
                                .bin_op   = { .type = type, .lhs = lhs, .rhs = rhs } } };

    return &ret->expr;
}

Expr* vertex_unary_op_new(A3CString span, UnaryOpType type, Expr* operand) {
    assert(operand);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_EXPR,
                      .expr = { .type     = EXPR_UNARY_OP,
                                .res_type = NULL,
                                .unary_op = { .type = type, .operand = operand } } };

    return &ret->expr;
}

Expr* vertex_lit_num_new(A3CString span, int64_t num) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) {
        .span = span,
        .type = V_EXPR,
        .expr = { .type = EXPR_LIT, .res_type = NULL, .lit = { .type = LIT_NUM, .num = num } }
    };

    return &ret->expr;
}

Item* vertex_expr_stmt_new(A3CString span, Expr* expr) {
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span, .type = V_STMT, .item = { .type = STMT_EXPR_STMT, .expr = expr } };

    return &ret->item;
}

Item* vertex_ret_new(A3CString span, Expr* expr) {
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .item = { .type = STMT_RET, .expr = expr } };

    return &ret->item;
}

Item* vertex_empty_new(A3CString span) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .item.type = STMT_EMPTY };

    return &ret->item;
}

Item* vertex_decl_new(A3CString span, A3CString name, PType* type) {
    assert(span.ptr);
    assert(name.ptr);
    assert(type);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_DECL, .item = { .name = name, .decl_ptype = type } };

    return &ret->item;
}

Block* vertex_block_new(void) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_STMT, .span = A3_CS_NULL, .item = { .type = STMT_BLOCK } };
    A3_SLL_INIT(&ret->item.block.body);

    return &ret->item.block;
}

Fn* vertex_fn_new(A3CString span, A3CString name, PType* type, Block* body) {
    assert(span.ptr);
    assert(name.ptr);
    assert(type);
    assert(body);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_FN,
                      .span = span,
                      .fn   = { .name = name, .body = body, .ptype = type } };

    return &ret->fn;
}

Expr* vertex_var_new(A3CString span) {
    assert(span.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span, .type = V_EXPR, .expr = { .type = EXPR_VAR, .var.name = span } };

    return &ret->expr;
}

Expr* vertex_call_new(A3CString span, A3CString name) {
    assert(span.ptr);
    assert(name.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span, .type = V_EXPR, .expr = { .type = EXPR_CALL, .call.name = name } };
    A3_SLL_INIT(&ret->expr.call.args);

    return &ret->expr;
}

If* vertex_if_new(A3CString span, Expr* cond, Item* body_true, Item* body_false) {
    assert(span.ptr);
    assert(cond);
    assert(body_true);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) {
        .span = span,
        .type = V_STMT,
        .item = { .type    = STMT_IF,
                  .if_stmt = { .cond = cond, .body_true = body_true, .body_false = body_false } }
    };

    return &ret->item.if_stmt;
}

Loop* vertex_loop_new(A3CString span, Item* init, Expr* cond, Expr* post, Item* body) {
    assert(span.ptr);
    assert(cond);
    assert(body);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span,
                   .type = V_STMT,
                   .item = { .type = STMT_LOOP,
                             .loop = { .init = init, .cond = cond, .post = post, .body = body } } };

    return &ret->item.loop;
}

Unit* vertex_unit_new(void) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_UNIT };
    A3_SLL_INIT(&ret->unit.fns);

    return &ret->unit;
}

static bool visit_bin_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->lhs, expr)));
    return vertex_visit(visitor, VERTEX(op->rhs, expr));
}

static bool visit_unary_op(AstVisitor* visitor, UnaryOp* op) {
    assert(visitor);
    assert(op);

    return vertex_visit(visitor, VERTEX(op->operand, expr));
}

static bool visit_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);
    assert(lit);

    (void)visitor;
    (void)lit;
    return true;
}

static bool visit_var(AstVisitor* visitor, Var* var) {
    assert(visitor);
    assert(var);

    (void)visitor;
    (void)var;
    return true;
}

static bool visit_expr_stmt(AstVisitor* visitor, Item* stmt) {
    assert(visitor);
    assert(stmt);
    assert(stmt->type == STMT_EXPR_STMT);

    return vertex_visit(visitor, VERTEX(stmt->expr, expr));
}

static bool visit_ret(AstVisitor* visitor, Item* stmt) {
    assert(visitor);
    assert(stmt);
    assert(stmt->type == STMT_RET);

    return vertex_visit(visitor, VERTEX(stmt->expr, expr));
}

static bool visit_if(AstVisitor* visitor, If* if_stmt) {
    assert(visitor);
    assert(if_stmt);

    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->cond, expr)));
    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_true, item)));
    if (if_stmt->body_false)
        return vertex_visit(visitor, VERTEX(if_stmt->body_false, item));
    return true;
}

static bool visit_block(AstVisitor* visitor, Block* block) {
    assert(visitor);
    assert(block);

    A3_SLL_FOR_EACH(Item, stmt, &block->body, link) {
        A3_TRYB(vertex_visit(visitor, VERTEX(stmt, item)));
    }

    return true;
}

static bool visit_loop(AstVisitor* visitor, Loop* loop) {
    assert(visitor);
    assert(loop);

    if (loop->init)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->init, item)));
    if (loop->cond)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->cond, expr)));
    A3_TRYB(vertex_visit(visitor, VERTEX(loop->body, item)));
    if (loop->post)
        return vertex_visit(visitor, VERTEX(loop->post, expr));
    return true;
}

static bool visit_decl(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(decl);
    assert(VERTEX(decl, item)->type == V_DECL);

    return true;
}

static bool visit_fn(AstVisitor* visitor, Fn* fn) {
    assert(visitor);
    assert(fn);

    return vertex_visit(visitor, VERTEX(fn->body, item.block));
}

static bool visit_call(AstVisitor* visitor, Call* call) {
    assert(visitor);
    assert(call);

    A3_SLL_FOR_EACH(Arg, arg, &call->args, link) {
        A3_TRYB(vertex_visit(visitor, VERTEX(arg->expr, expr)));
    }

    return true;
}

#define VISIT(VISITOR, NAME, VERTEX) ((((VISITOR)->NAME) ?: NAME)((VISITOR), (VERTEX)))

bool vertex_visit(AstVisitor* visitor, Vertex* vertex) {
    assert(visitor);
    assert(vertex);

    switch (vertex->type) {
    case V_UNIT:
        A3_SLL_FOR_EACH(Fn, fn, &vertex->unit.fns, link) {
            A3_TRYB(vertex_visit(visitor, VERTEX(fn, fn)));
        }
        return true;
    case V_EXPR:
        switch (vertex->expr.type) {
        case EXPR_BIN_OP:
            return VISIT(visitor, visit_bin_op, &vertex->expr.bin_op);
        case EXPR_UNARY_OP:
            return VISIT(visitor, visit_unary_op, &vertex->expr.unary_op);
        case EXPR_LIT:
            return VISIT(visitor, visit_lit, &vertex->expr.lit);
        case EXPR_VAR:
            return VISIT(visitor, visit_var, &vertex->expr.var);
        case EXPR_CALL:
            return VISIT(visitor, visit_call, &vertex->expr.call);
        }
        break;
    case V_STMT:
        switch (vertex->item.type) {
        case STMT_EXPR_STMT:
            return VISIT(visitor, visit_expr_stmt, &vertex->item);
        case STMT_RET:
            return VISIT(visitor, visit_ret, &vertex->item);
        case STMT_IF:
            return VISIT(visitor, visit_if, &vertex->item.if_stmt);
        case STMT_BLOCK:
            return VISIT(visitor, visit_block, &vertex->item.block);
        case STMT_EMPTY:
            return true;
        case STMT_LOOP:
            return VISIT(visitor, visit_loop, &vertex->item.loop);
        }
        break;
    case V_DECL:
        return VISIT(visitor, visit_decl, &vertex->item);
    case V_FN:
        return VISIT(visitor, visit_fn, &vertex->fn);
    }

    A3_UNREACHABLE();
}

PType* ptype_builtin_new(TokenType type) {
    assert(type == TOK_INT);

    A3_UNWRAPNI(PType*, ret, calloc(1, sizeof(*ret)));
    *ret = (PType) { .type = PTY_BUILTIN, .builtin = type };

    return ret;
}

PType* ptype_ptr_to(PType* type) {
    assert(type);

    A3_UNWRAPNI(PType*, ret, calloc(1, sizeof(*ret)));
    *ret = (PType) { .type = PTY_PTR, .parent = type };

    return ret;
}

PType* ptype_fn(PType* ret_type) {
    assert(ret_type);

    A3_UNWRAPNI(PType*, ret, calloc(1, sizeof(*ret)));
    *ret = (PType) { .type = PTY_FN, .ret = ret_type };

    return ret;
}

Arg* arg_new(Expr* expr) {
    assert(expr);

    A3_UNWRAPNI(Arg*, ret, calloc(1, sizeof(*ret)));
    *ret = (Arg) { .expr = expr };

    return ret;
}

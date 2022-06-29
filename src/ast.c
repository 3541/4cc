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

Statement* vertex_expr_stmt_new(A3CString span, Expr* expr) {
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span, .type = V_STMT, .stmt = { .type = STMT_EXPR_STMT, .expr = expr } };

    return &ret->stmt;
}

Statement* vertex_ret_new(A3CString span, Expr* expr) {
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .stmt = { .type = STMT_RET, .expr = expr } };

    return &ret->stmt;
}

Statement* vertex_empty_new(A3CString span) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .stmt.type = STMT_EMPTY };

    return &ret->stmt;
}

Block* vertex_block_new(void) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_STMT, .span = A3_CS_NULL, .stmt = { .type = STMT_BLOCK } };
    A3_SLL_INIT(&ret->stmt.block.body);

    return &ret->stmt.block;
}

Fn* vertex_fn_new(A3CString name, Block* body) {
    assert(name.ptr);
    assert(body);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_FN, .span = name, .fn = { .name = name, .body = body } };

    return &ret->fn;
}

Expr* vertex_var_new(A3CString span) {
    assert(span.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span, .type = V_EXPR, .expr = { .type = EXPR_VAR, .var.name = span } };
    return &ret->expr;
}

If* vertex_if_new(A3CString span, Expr* cond, Statement* body_true, Statement* body_false) {
    assert(span.ptr);
    assert(cond);
    assert(body_true);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) {
        .span = span,
        .type = V_STMT,
        .stmt = { .type    = STMT_IF,
                  .if_stmt = { .cond = cond, .body_true = body_true, .body_false = body_false } }
    };

    return &ret->stmt.if_stmt;
}

Loop* vertex_loop_new(A3CString span, Statement* init, Expr* cond, Expr* post, Statement* body) {
    assert(span.ptr);
    assert(cond);
    assert(body);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span,
                   .type = V_STMT,
                   .stmt = { .type = STMT_LOOP,
                             .loop = { .init = init, .cond = cond, .post = post, .body = body } } };

    return &ret->stmt.loop;
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

static bool visit_expr_stmt(AstVisitor* visitor, Statement* stmt) {
    assert(visitor);
    assert(stmt);
    assert(stmt->type == STMT_EXPR_STMT);

    return vertex_visit(visitor, VERTEX(stmt->expr, expr));
}

static bool visit_ret(AstVisitor* visitor, Statement* stmt) {
    assert(visitor);
    assert(stmt);
    assert(stmt->type == STMT_RET);

    return vertex_visit(visitor, VERTEX(stmt->expr, expr));
}

static bool visit_if(AstVisitor* visitor, If* if_stmt) {
    assert(visitor);
    assert(if_stmt);

    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->cond, expr)));
    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_true, stmt)));
    if (if_stmt->body_false)
        return vertex_visit(visitor, VERTEX(if_stmt->body_false, stmt));
    return true;
}

static bool visit_block(AstVisitor* visitor, Block* block) {
    assert(visitor);
    assert(block);

    A3_SLL_FOR_EACH(Statement, stmt, &block->body, link) {
        A3_TRYB(vertex_visit(visitor, VERTEX(stmt, stmt)));
    }

    return true;
}

static bool visit_loop(AstVisitor* visitor, Loop* loop) {
    assert(visitor);
    assert(loop);

    if (loop->init)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->init, stmt)));
    if (loop->cond)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->cond, expr)));
    A3_TRYB(vertex_visit(visitor, VERTEX(loop->body, stmt)));
    if (loop->post)
        return vertex_visit(visitor, VERTEX(loop->post, expr));
    return true;
}

static bool visit_fn(AstVisitor* visitor, Fn* fn) {
    assert(visitor);
    assert(fn);

    return vertex_visit(visitor, VERTEX(fn->body, stmt.block));
}

#define VISIT(VISITOR, NAME, VERTEX) ((((VISITOR)->NAME) ?: NAME)((VISITOR), (VERTEX)))

bool vertex_visit(AstVisitor* visitor, Vertex* vertex) {
    assert(visitor);
    assert(vertex);

    switch (vertex->type) {
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
        }
        break;
    case V_STMT:
        switch (vertex->stmt.type) {
        case STMT_EXPR_STMT:
            return VISIT(visitor, visit_expr_stmt, &vertex->stmt);
        case STMT_RET:
            return VISIT(visitor, visit_ret, &vertex->stmt);
        case STMT_IF:
            return VISIT(visitor, visit_if, &vertex->stmt.if_stmt);
        case STMT_BLOCK:
            return VISIT(visitor, visit_block, &vertex->stmt.block);
        case STMT_EMPTY:
            return true;
        case STMT_LOOP:
            return VISIT(visitor, visit_loop, &vertex->stmt.loop);
        }
        break;
    case V_FN:
        return VISIT(visitor, visit_fn, &vertex->fn);
    }

    A3_UNREACHABLE();
}

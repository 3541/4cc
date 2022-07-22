/*
 * EVAL -- Compile-time expression evaluation.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include "eval.h"

#include <assert.h>
#include <stdarg.h>

#include "ast.h"
#include "error.h"
#include "type.h"

typedef struct EvalCtx {
    A3CString src;
    int64_t   ret;
} EvalCtx;

static void eval_error(EvalCtx* ctx, Vertex* vertex) {
    assert(ctx);
    assert(vertex);

    error_at(ctx->src, vertex->span, "Not supported at compile-time.");
}

static bool eval_bin_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    EvalCtx* ctx = visitor->ctx;

    if (op->type == OP_ASSIGN || op->type == OP_OR || op->type == OP_AND || op->type == OP_CAST) {
        eval_error(ctx, VERTEX(op, expr.bin_op));
        return false;
    }

    A3_TRYB(vertex_visit(visitor, VERTEX(op->lhs, expr)));
    int64_t lhs = ctx->ret;

    A3_TRYB(vertex_visit(visitor, VERTEX(op->rhs, expr)));
    int64_t rhs = ctx->ret;

    switch (op->type) {
    case OP_ADD:
        ctx->ret = lhs + rhs;
        break;
    case OP_SUB:
        ctx->ret = lhs - rhs;
        break;
    case OP_MUL:
        ctx->ret = lhs * rhs;
        break;
    case OP_DIV:
        ctx->ret = lhs / rhs;
        break;
    case OP_MOD:
        ctx->ret = lhs % rhs;
        break;
    case OP_EQ:
        ctx->ret = lhs == rhs;
        break;
    case OP_NE:
        ctx->ret = lhs != rhs;
        break;
    case OP_LT:
        ctx->ret = lhs < rhs;
        break;
    case OP_LE:
        ctx->ret = lhs <= rhs;
        break;
    case OP_GT:
        ctx->ret = lhs > rhs;
        break;
    case OP_GE:
        ctx->ret = lhs >= rhs;
        break;
    case OP_SHL:
        ctx->ret = lhs << rhs;
        break;
    case OP_SHR:
        ctx->ret = lhs >> rhs;
        break;
    case OP_BW_AND:
        ctx->ret = lhs & rhs;
        break;
    case OP_BW_OR:
        ctx->ret = lhs | rhs;
        break;
    case OP_BW_XOR:
        ctx->ret = lhs ^ rhs;
        break;
    case OP_AND:
    case OP_OR:
    case OP_ASSIGN:
    case OP_CAST:
        eval_error(ctx, VERTEX(op, expr.bin_op));
        return false;
    }

    return true;
}

static bool eval_call(AstVisitor* visitor, Call* call) {
    assert(visitor);
    assert(call);

    eval_error(visitor->ctx, VERTEX(call, expr.call));
    return false;
}

static bool eval_expr_cond(AstVisitor* visitor, CondExpr* expr) {
    assert(visitor);
    assert(expr);

    EvalCtx* ctx = visitor->ctx;

    A3_TRYB(vertex_visit(visitor, VERTEX(expr->cond, expr)));
    if (ctx->ret)
        return vertex_visit(visitor, expr->res_true ? VERTEX(expr->res_true, expr)
                                                    : VERTEX(expr->cond, expr));

    return vertex_visit(visitor, VERTEX(expr->res_false, expr));
}

static bool eval_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);
    assert(lit);

    if (lit->type != LIT_NUM) {
        eval_error(visitor->ctx, VERTEX(lit, expr.lit));
        return false;
    }

    ((EvalCtx*)visitor->ctx)->ret = lit->num;
    return true;
}

static bool eval_member(AstVisitor* visitor, MemberAccess* mem) {
    assert(visitor);
    assert(mem);

    eval_error(visitor->ctx, VERTEX(mem, expr.member));
    return false;
}

static bool eval_unary_op(AstVisitor* visitor, UnaryOp* op) {
    assert(visitor);
    assert(op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->operand, expr)));

    EvalCtx* ctx = visitor->ctx;

    switch (op->type) {
    case OP_UNARY_ADD:
        break;
    case OP_NEG:
        ctx->ret = -ctx->ret;
        break;
    case OP_NOT:
        ctx->ret = !ctx->ret;
        break;
    case OP_BW_NOT:
        ctx->ret = ~ctx->ret;
        break;
    case OP_SIZEOF:
        ctx->ret = (int64_t)op->operand->res_type->size;
        break;
    case OP_DEREF:
    case OP_ADDR:
        eval_error(ctx, VERTEX(op, expr.unary_op));
        return false;
    }

    return true;
}

static bool eval_var(AstVisitor* visitor, Var* var) {
    assert(visitor);
    assert(var);

    eval_error(visitor->ctx, VERTEX(var, expr.var));
    return false;
}

EvalResult eval(A3CString src, Expr const* expr) {
    assert(expr);

    EvalCtx ctx = { .src = src, .ret = 0 };

    return (EvalResult) {
        .ok = vertex_visit(
            &(AstVisitor) {
                .ctx             = &ctx,
                .visit_bin_op    = eval_bin_op,
                .visit_call      = eval_call,
                .visit_expr_cond = eval_expr_cond,
                .visit_lit       = eval_lit,
                .visit_member    = eval_member,
                .visit_unary_op  = eval_unary_op,
                .visit_var       = eval_var,
            },
            VERTEX(expr, expr)),
        .value = ctx.ret,
    };
}

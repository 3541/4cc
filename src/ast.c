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

A3_HT_DECLARE_METHODS(A3CString, Var)
A3_HT_DEFINE_METHODS(A3CString, Var, a3_string_cptr, a3_string_len, a3_string_cmp)

Scope* scope_new(Scope* parent) {
    A3_UNWRAPNI(Scope*, ret, calloc(1, sizeof(*ret)));
    *ret = (Scope) { .parent = parent, .stack_depth = parent ? parent->stack_depth : 0 };
    A3_HT_INIT(A3CString, Var)(&ret->scope, A3_HT_NO_HASH_KEY, A3_HT_ALLOW_GROWTH);

    return ret;
}

Vertex* vertex_bin_op_new(A3CString span, BinOpType type, Vertex* lhs, Vertex* rhs) {
    assert(lhs);
    assert(rhs);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span   = span,
                      .type   = V_BIN_OP,
                      .bin_op = { .type = type, .lhs = lhs, .rhs = rhs } };

    return ret;
}

Vertex* vertex_unary_op_new(A3CString span, UnaryOpType type, Vertex* operand) {
    assert(operand);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span     = span,
                      .type     = V_UNARY_OP,
                      .unary_op = { .type = type, .operand = operand } };

    return ret;
}

Vertex* vertex_lit_num_new(A3CString span, int64_t num) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_LIT, .lit = { .type = LIT_NUM, .num = num } };

    return ret;
}

Vertex* vertex_expr_stmt_new(A3CString span, Vertex* expr) {
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span, .type = V_STMT, .stmt = { .type = STMT_EXPR_STMT, .expr = expr } };

    return ret;
}

Vertex* vertex_ret_new(A3CString span, Vertex* expr) {
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .stmt = { .type = STMT_RET, .expr = expr } };

    return ret;
}

Vertex* vertex_empty_new(A3CString span) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .stmt.type = STMT_EMPTY };

    return ret;
}

Vertex* vertex_block_new(Scope* scope) {
    assert(scope);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_STMT,
                      .span = A3_CS_NULL,
                      .stmt = { .type = STMT_BLOCK, .block.scope = scope } };
    A3_SLL_INIT(&ret->stmt.block.body);

    return ret;
}

Vertex* vertex_fn_new(A3CString name, Vertex* body) {
    assert(name.ptr);
    assert(body);
    assert(body->type == V_STMT && body->stmt.type == STMT_BLOCK);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_FN, .span = name, .fn = { .name = name, .body = body } };

    return ret;
}

Vertex* vertex_var_new(A3CString span, Scope* scope) {
    assert(span.ptr);
    assert(scope);

    Var* var = A3_HT_FIND(A3CString, Var)(&scope->scope, span);
    if (!var) {
        A3_UNWRAPN(var, calloc(1, sizeof(*var)));
        Var new_var = { .name = span, .stack_offset = scope->stack_depth };
        scope->stack_depth += sizeof(int64_t);
        A3_HT_INSERT(A3CString, Var)(&scope->scope, span, new_var);
        var = A3_HT_FIND(A3CString, Var)(&scope->scope, span);
    }

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_VAR, .var = var };
    return ret;
}

bool vertex_visit(AstVisitor* visitor, Vertex* vertex) {
    assert(visitor);
    assert(vertex);

    switch (vertex->type) {
    case V_LIT:
        return visitor->visit_lit(visitor, &vertex->lit);
    case V_BIN_OP:
        return visitor->visit_bin_op(visitor, &vertex->bin_op);
    case V_UNARY_OP:
        return visitor->visit_unary_op(visitor, &vertex->unary_op);
    case V_STMT:
        switch (vertex->stmt.type) {
        case STMT_EXPR_STMT:
            return visitor->visit_expr_stmt(visitor, &vertex->stmt);
        case STMT_RET:
            return visitor->visit_ret(visitor, &vertex->stmt);
        case STMT_BLOCK:
            return visitor->visit_block(visitor, &vertex->stmt.block);
        case STMT_EMPTY:
            return true;
        }
        break;
    case V_VAR:
        return visitor->visit_var(visitor, vertex->var);
    case V_FN:
        return visitor->visit_fn(visitor, &vertex->fn);
    }

    A3_UNREACHABLE();
}

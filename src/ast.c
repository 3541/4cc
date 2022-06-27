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

Vertex* vertex_bin_op_new(A3CString span, BinOpType type, Vertex* lhs, Vertex* rhs) {
    assert(lhs);
    assert(rhs);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_BIN_OP, .bin_op_type = type, .lhs = lhs, .rhs = rhs };

    return ret;
}

Vertex* vertex_unary_op_new(A3CString span, UnaryOpType type, Vertex* operand) {
    assert(operand);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_UNARY_OP, .unary_op_type = type, .operand = operand };

    return ret;
}

Vertex* vertex_lit_num_new(A3CString span, int64_t num) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_LIT, .lit_type = LIT_NUM, .lit_num = num };

    return ret;
}

Vertex* vertex_expr_stmt_new(A3CString span, Vertex* expr) {
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .stmt_type = STMT_EXPR_STMT, .expr = expr };

    return ret;
}

bool vertex_visit(AstVisitor* visitor, Vertex* vertex) {
    assert(visitor);
    assert(vertex);

    AstVisitorCallback visit = visitor->visit_lit;

    switch (vertex->type) {
    case V_LIT:
        visit = visitor->visit_lit;
        break;
    case V_BIN_OP:
        visit = visitor->visit_bin_op;
        break;
    case V_UNARY_OP:
        visit = visitor->visit_unary_op;
        break;
    case V_STMT:
        assert(vertex->stmt_type == STMT_EXPR_STMT);
        visit = visitor->visit_expr_stmt;
        break;
    case V_VAR:
        visit = visitor->visit_var;
        break;
    case V_FN:
        visit = visitor->visit_fn;
        break;
    }

    return visit(visitor, vertex);
}

Vertex* vertex_fn_new(A3CString name) {
    assert(name.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_FN, .span = name, .fn = { .name = name, .stack_depth = 0 } };
    A3_SLL_INIT(&ret->fn.body);
    A3_HT_INIT(A3CString, Var)(&ret->fn.scope, A3_HT_NO_HASH_KEY, true);

    return ret;
}

Vertex* vertex_var_new(A3CString span, Fn* scope) {
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

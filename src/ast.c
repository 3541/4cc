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

#include <a3/sll.h>
#include <a3/str.h>
#include <a3/util.h>

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

void vertex_visit(AstVisitor* visitor, Vertex* vertex) {
    assert(visitor);
    assert(vertex);

    switch (vertex->type) {
    case V_LIT:
        visitor->visit_lit(visitor, vertex);
        break;
    case V_BIN_OP:
        visitor->visit_bin_op(visitor, vertex);
        break;
    case V_UNARY_OP:
        visitor->visit_unary_op(visitor, vertex);
        break;
    case V_STMT:
        switch (vertex->stmt_type) {
        case STMT_EXPR_STMT:
            visitor->visit_expr_stmt(visitor, vertex);
            break;
        }

        if (A3_SLL_NEXT(vertex, link))
            vertex_visit(visitor, A3_SLL_NEXT(vertex, link));
        break;
    }
}

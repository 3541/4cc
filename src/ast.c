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
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <a3/ll.h>
#include <a3/sll.h>
#include <a3/str.h>
#include <a3/util.h>

#include "type.h"
#include "util.h"

Expr* vertex_bin_op_new(Span span, BinOpType type, Expr* lhs, Expr* rhs) {
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

Expr* vertex_unary_op_new(Span span, UnaryOpType type, Expr* operand) {
    assert(operand);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_EXPR,
                      .expr = { .type     = EXPR_UNARY_OP,
                                .res_type = NULL,
                                .unary_op = { .type = type, .operand = operand } } };

    return &ret->expr;
}

Expr* vertex_lit_num_new(Span span, Type const* type, uintmax_t num) {
    assert(span.text.ptr);
    assert(type);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) {
        .span = span,
        .type = V_EXPR,
        .expr = { .type = EXPR_LIT, .res_type = type, .lit = { .type = LIT_NUM, .num = num } }
    };

    return &ret->expr;
}

Expr* vertex_lit_str_new(Span span, A3CString str) {
    assert(span.text.ptr);
    assert(str.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) {
        .span = span,
        .type = V_EXPR,
        .expr = { .type = EXPR_LIT, .res_type = NULL, .lit = { .type = LIT_STR, .str = str } }
    };

    return &ret->expr;
}

Expr* vertex_expr_type_new(Span span, PType* type) {
    assert(span.text.ptr);
    assert(type);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span, .type = V_EXPR, .expr = { .type = EXPR_TYPE, .res_ptype = type } };

    return &ret->expr;
}

Item* vertex_expr_stmt_new(Span span, Expr* expr) {
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span, .type = V_STMT, .item = { .type = STMT_EXPR_STMT, .expr = expr } };

    return &ret->item;
}

Item* vertex_ret_new(Span span, Expr* expr) {
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .item = { .type = STMT_RET, .expr = expr } };

    return &ret->item;
}

Item* vertex_empty_new(Span span) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .item.type = STMT_EMPTY };

    return &ret->item;
}

Item* vertex_break_continue_new(Span span, StmtType type) {
    assert(span.text.ptr);
    assert(type == STMT_BREAK || type == STMT_CONTINUE);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_STMT, .item.type = type };

    return &ret->item;
}

Item* vertex_decl_new(Span span, A3CString name, PType* type) {
    assert(type);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) {
        .span = span,
        .type = V_DECL,
        .item = { .name = name, .obj = NULL, .attributes = type->attributes, .decl_ptype = type }
    };

    return &ret->item;
}

Block* vertex_block_new(void) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_STMT, .item = { .type = STMT_BLOCK } };
    A3_SLL_INIT(&ret->item.block.body);

    return &ret->item.block;
}

Expr* vertex_var_new(Span span, A3CString name) {
    assert(span.text.ptr);
    assert(name.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Vertex) { .span = span, .type = V_EXPR, .expr = { .type = EXPR_VAR, .var.name = name } };

    return &ret->expr;
}

Expr* vertex_call_new(Span span, Expr* callee) {
    assert(span.text.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_EXPR,
                      .expr = { .type = EXPR_CALL, .call = { .callee = callee, .arg_count = 0 } } };
    A3_LL_INIT(&ret->expr.call.args, link);

    return &ret->expr;
}

Expr* vertex_member_new(Span span, Expr* lhs, A3CString rhs_name) {
    assert(span.text.ptr);
    assert(lhs);
    assert(rhs_name.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_EXPR,
                      .expr = { .type = EXPR_MEMBER, .member = { .lhs = lhs, .name = rhs_name } } };

    return &ret->expr;
}

Expr* vertex_expr_cond_new(Span span, Expr* cond, Expr* res_true, Expr* res_false) {
    assert(span.text.ptr);
    assert(cond);
    assert(res_false);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_EXPR,
                      .expr = {
                          .type = EXPR_COND,
                          .cond = { .cond = cond, .res_true = res_true, .res_false = res_false },
                      } };

    return &ret->expr;
}

If* vertex_if_new(Span span, Expr* cond, Item* body_true, Item* body_false) {
    assert(span.text.ptr);
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

Loop* vertex_loop_new(Span span, bool cond_pos, Item* init, Expr* cond, Expr* post, Item* body) {
    assert(span.text.ptr);
    assert(body);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_STMT,
                      .item = { .type = STMT_LOOP,
                                .loop = { .init       = init,
                                          .cond       = cond,
                                          .post       = post,
                                          .body       = body,
                                          .cond_first = cond_pos } } };

    return &ret->item.loop;
}

Unit* vertex_unit_new(void) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_UNIT };
    A3_SLL_INIT(&ret->unit.items);

    return &ret->unit;
}

Init* vertex_init_expr_new(Span span, Expr* expr) {
    assert(span.text.ptr);
    assert(expr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span, .type = V_INIT, .init = { .type = INIT_EXPR, .expr = expr } };

    return &ret->init;
}

Init* vertex_init_list_new(void) {
    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .type = V_INIT, .init.type = INIT_LIST };
    A3_SLL_INIT(&ret->init.list);

    return &ret->init;
}

Item* vertex_goto_new(Span span, A3CString label) {
    assert(span.text.ptr);
    assert(label.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_STMT,
                      .item = { .type = STMT_GOTO, .jmp = { .label = label, .target = NULL } } };

    return &ret->item;
}

Item* vertex_label_new(Span span, A3CString label) {
    assert(span.text.ptr);
    assert(label.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) {
        .span = span,
        .type = V_STMT,
        .item = { .type  = STMT_LABELED,
                  .label = { .is_switch_label = false, .name = label, .label = util_ident() } }
    };

    return &ret->item;
}

Item* vertex_case_label_new(Span span, Expr* expr) {
    assert(span.text.ptr);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) {
        .span = span,
        .type = V_STMT,
        .item = { .type  = STMT_LABELED,
                  .label = { .is_switch_label = true, .expr = expr, .label = util_ident() } }
    };

    return &ret->item;
}

Switch* vertex_switch_new(Span span, Expr* cond) {
    assert(span.text.ptr);
    assert(cond);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_STMT,
                      .item = {
                          .type        = STMT_SWITCH,
                          .switch_stmt = { .cond = cond, .default_case = NULL, .body = NULL } } };
    A3_SLL_INIT(&ret->item.switch_stmt.cases);

    return &ret->item.switch_stmt;
}

void vertex_init_lit_str_to_list(Init* init) {
    assert(init);
    assert(init->type == INIT_EXPR && init->expr->type == EXPR_LIT &&
           init->expr->lit.type == LIT_STR);

    Literal* lit = &init->expr->lit;

    init->type = INIT_LIST;
    A3_SLL_INIT(&init->list);

    Span span = SPAN(init, init);
    for (size_t i = 0; i < lit->str.len; i++) {
        Init* init_char = vertex_init_expr_new(
            span, vertex_lit_num_new(span, BUILTIN_TYPES[TY_U8], lit->str.ptr[i]));

        A3_SLL_ENQUEUE(&init->list, init_char, link);
    }

    Init* init_null = vertex_init_expr_new(span, vertex_lit_num_new(span, BUILTIN_TYPES[TY_U8], 0));
    A3_SLL_ENQUEUE(&init->list, init_null, link);
}

PType* ptype_builtin_new(Span span, PTypeBuiltinType type) {
    assert(span.text.ptr);
    assert(type == PTY_VOID || !(type & PTY_VOID));

    A3_UNWRAPNI(PType*, ret, calloc(1, sizeof(*ret)));
    *ret = (PType) { .type = PTY_BUILTIN, .span = span, .attributes = { 0 }, .builtin_type = type };

    return ret;
}

PType* ptype_ptr_new(Span span, PType* type) {
    assert(span.text.ptr);
    assert(type);

    A3_UNWRAPNI(PType*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (PType) { .type = PTY_PTR, .span = span, .attributes = type->attributes, .parent = type };

    return ret;
}

PType* ptype_fn_new(Span span, PType* ret_type) {
    assert(span.text.ptr);
    assert(ret_type);

    A3_UNWRAPNI(PType*, ret, calloc(1, sizeof(*ret)));
    *ret = (PType) {
        .type = PTY_FN, .span = span, .attributes = ret_type->attributes, .ret = ret_type
    };
    A3_SLL_INIT(&ret->params);

    return ret;
}

PType* ptype_array_new(Span span, PType* base, Expr* len) {
    assert(span.text.ptr);
    assert(base);

    A3_UNWRAPNI(PType*, ret, calloc(1, sizeof(*ret)));
    *ret = (PType) {
        .type = PTY_ARRAY, .span = span, .attributes = base->attributes, .parent = base, .len = len
    };

    return ret;
}

PType* ptype_aggregate_new(Span span, PTypeType type, Span name) {
    assert(span.text.ptr);
    assert(type == PTY_STRUCT || type == PTY_UNION || type == PTY_ENUM);

    A3_UNWRAPNI(PType*, ret, calloc(1, sizeof(*ret)));
    *ret = (PType) { .type = type, .span = span, .attributes = { 0 }, .name = name };
    A3_SLL_INIT(&ret->members);

    return ret;
}

PType* ptype_defined_new(Span name) {
    assert(name.text.ptr);

    A3_UNWRAPNI(PType*, ret, calloc(1, sizeof(*ret)));
    *ret = (PType) {
        .type = PTY_DEFINED, .span = name, .defined_name = name.text, .builtin_type = PTY_NOTHING
    };

    return ret;
}

Arg* arg_new(Expr* expr) {
    assert(expr);

    A3_UNWRAPNI(Arg*, ret, calloc(1, sizeof(*ret)));
    *ret = (Arg) { .expr = expr };

    return ret;
}

Member* member_new(A3CString name, PType* type) {
    assert(type);

    A3_UNWRAPNI(Member*, ret, calloc(1, sizeof(*ret)));
    *ret = (Member) { .name = name, .ptype = type };

    return ret;
}

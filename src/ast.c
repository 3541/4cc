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
                      .expr = { .type = EXPR_CALL, .call.callee = callee } };
    A3_SLL_INIT(&ret->expr.call.args);

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

Item* vertex_label_new(Span span, A3CString label, Item* stmt) {
    assert(span.text.ptr);
    assert(label.ptr);
    assert(stmt);
    assert(VERTEX(stmt, item)->type == V_STMT);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_STMT,
                      .item = { .type  = STMT_LABELED,
                                .label = { .is_switch_label = false,
                                           .name            = label,
                                           .stmt            = stmt,
                                           .label           = util_ident() } } };

    return &ret->item;
}

Item* vertex_case_label_new(Span span, Expr* expr, Item* stmt) {
    assert(span.text.ptr);
    assert(stmt);
    assert(VERTEX(stmt, item)->type == V_STMT);

    A3_UNWRAPNI(Vertex*, ret, calloc(1, sizeof(*ret)));
    *ret = (Vertex) { .span = span,
                      .type = V_STMT,
                      .item = { .type  = STMT_LABELED,
                                .label = { .is_switch_label = true,
                                           .expr            = expr,
                                           .stmt            = stmt,
                                           .label           = util_ident() } } };

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

static bool visit_bin_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->lhs, expr)));
    A3_TRYB(vertex_visit(visitor, VERTEX(op->rhs, expr)));

    return true;
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

    A3_SLL_FOR_EACH (Item, stmt, &block->body, link) {
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

    if (decl->body)
        return vertex_visit(visitor, VERTEX(decl->body, item.block));

    return true;
}

static bool visit_call(AstVisitor* visitor, Call* call) {
    assert(visitor);
    assert(call);

    A3_SLL_FOR_EACH (Arg, arg, &call->args, link) {
        A3_TRYB(vertex_visit(visitor, VERTEX(arg->expr, expr)));
    }

    return true;
}

static bool visit_member(AstVisitor* visitor, MemberAccess* member) {
    assert(visitor);
    assert(member);
    (void)member;

    return vertex_visit(visitor, VERTEX(member->lhs, expr));
}

static bool visit_expr_cond(AstVisitor* visitor, CondExpr* expr) {
    assert(visitor);
    assert(expr);

    A3_TRYB(vertex_visit(visitor, VERTEX(expr->cond, expr)));
    if (expr->res_true)
        A3_TRYB(vertex_visit(visitor, VERTEX(expr->res_true, expr)));
    return vertex_visit(visitor, VERTEX(expr->res_false, expr));
}

static bool visit_expr_type(AstVisitor* visitor, Expr* expr) {
    assert(visitor);
    assert(expr);
    assert(expr->type == EXPR_TYPE);

    (void)visitor;
    (void)expr;

    return true;
}

static bool visit_break_continue(AstVisitor* visitor, Item* item) {
    assert(visitor);
    assert(item);
    (void)visitor;
    (void)item;

    return true;
}

static bool visit_init(AstVisitor* visitor, Init* init) {
    assert(visitor);
    assert(init);

    switch (init->type) {
    case INIT_EXPR:
        return vertex_visit(visitor, VERTEX(init->expr, expr));
    case INIT_LIST:
        A3_SLL_FOR_EACH (Init, elem, &init->list, link)
            A3_TRYB(vertex_visit(visitor, VERTEX(elem, init)));
        return true;
    }

    A3_UNREACHABLE();
}

static bool visit_goto(AstVisitor* visitor, Goto* jmp) {
    assert(visitor);
    assert(jmp);
    (void)visitor;
    (void)jmp;

    return true;
}

static bool visit_label(AstVisitor* visitor, Label* label) {
    assert(visitor);
    assert(label);

    return vertex_visit(visitor, VERTEX(label->stmt, item));
}

static bool visit_switch(AstVisitor* visitor, Switch* switch_stmt) {
    assert(visitor);
    assert(switch_stmt);

    A3_TRYB(vertex_visit(visitor, VERTEX(switch_stmt->cond, expr)));
    return vertex_visit(visitor, VERTEX(switch_stmt->body, item));
}

#define VISIT(VISITOR, NAME, VERTEX) ((((VISITOR)->NAME) ?: NAME)((VISITOR), (VERTEX)))

bool vertex_visit(AstVisitor* visitor, Vertex* vertex) {
    assert(visitor);
    assert(vertex);

    if (visitor->pre)
        visitor->pre(visitor, vertex);

    switch (vertex->type) {
    case V_UNIT:
        A3_SLL_FOR_EACH (Item, item, &vertex->unit.items, link) {
            A3_TRYB(vertex_visit(visitor, VERTEX(item, item)));
        }
        break;
    case V_EXPR:
        switch (vertex->expr.type) {
        case EXPR_BIN_OP:
            A3_TRYB(VISIT(visitor, visit_bin_op, &vertex->expr.bin_op));
            break;
        case EXPR_UNARY_OP:
            A3_TRYB(VISIT(visitor, visit_unary_op, &vertex->expr.unary_op));
            break;
        case EXPR_LIT:
            A3_TRYB(VISIT(visitor, visit_lit, &vertex->expr.lit));
            break;
        case EXPR_VAR:
            A3_TRYB(VISIT(visitor, visit_var, &vertex->expr.var));
            break;
        case EXPR_CALL:
            A3_TRYB(VISIT(visitor, visit_call, &vertex->expr.call));
            break;
        case EXPR_MEMBER:
            A3_TRYB(VISIT(visitor, visit_member, &vertex->expr.member));
            break;
        case EXPR_COND:
            A3_TRYB(VISIT(visitor, visit_expr_cond, &vertex->expr.cond));
            break;
        case EXPR_TYPE:
            A3_TRYB(VISIT(visitor, visit_expr_type, &vertex->expr));
            break;
        }
        break;
    case V_STMT:
        switch (vertex->item.type) {
        case STMT_EXPR_STMT:
            A3_TRYB(VISIT(visitor, visit_expr_stmt, &vertex->item));
            break;
        case STMT_RET:
            A3_TRYB(VISIT(visitor, visit_ret, &vertex->item));
            break;
        case STMT_IF:
            A3_TRYB(VISIT(visitor, visit_if, &vertex->item.if_stmt));
            break;
        case STMT_BLOCK:
            A3_TRYB(VISIT(visitor, visit_block, &vertex->item.block));
            break;
        case STMT_EMPTY:
            break;
        case STMT_BREAK:
        case STMT_CONTINUE:
            A3_TRYB(VISIT(visitor, visit_break_continue, &vertex->item));
            break;
        case STMT_LOOP:
            A3_TRYB(VISIT(visitor, visit_loop, &vertex->item.loop));
            break;
        case STMT_GOTO:
            A3_TRYB(VISIT(visitor, visit_goto, &vertex->item.jmp));
            break;
        case STMT_LABELED:
            A3_TRYB(VISIT(visitor, visit_label, &vertex->item.label));
            break;
        case STMT_SWITCH:
            A3_TRYB(VISIT(visitor, visit_switch, &vertex->item.switch_stmt));
            break;
        }
        break;
    case V_DECL:
        A3_TRYB(VISIT(visitor, visit_decl, &vertex->item));
        break;
    case V_INIT:
        A3_TRYB(VISIT(visitor, visit_init, &vertex->init));
        break;
    }

    return true;
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

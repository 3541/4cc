/*
 * VISIT -- Syntax tree traversal.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include <assert.h>
#include <stdbool.h>

#include <a3/ll.h>
#include <a3/sll.h>
#include <a3/util.h>

#include "ast.h"

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

    A3_LL_FOR_EACH (Arg, arg, &call->args, link) {
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

    return true;
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

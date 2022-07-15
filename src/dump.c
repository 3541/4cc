/*
 * DUMP -- AST printing.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include "dump.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>

#include <a3/util.h>

#include "ast.h"
#include "type.h"

typedef struct Dumper {
    size_t indent;
} Dumper;

A3_FORMAT_FN(2, 3)
static void dump_print(Dumper* d, char* fmt, ...) {
    assert(d);

    fprintf(stderr, "%*s", (int)d->indent, "");

    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    fputc('\n', stderr);
}

static bool dump_child(AstVisitor* visitor, Vertex* child) {
    assert(visitor);
    assert(child);

    Dumper* d = visitor->ctx;

    d->indent++;
    bool res = vertex_visit(visitor, child);
    d->indent--;

    return res;
}

static A3String dump_get_type(Type const* type) {
    if (!type)
        return a3_string_clone(A3_CS("(untyped)"));

    A3String ret = type_name(type);
    if (!ret.ptr)
        return a3_string_clone(A3_CS("(untyped)"));

    return ret;
}

static A3String dump_obj_type(Obj* obj) {
    if (!obj)
        return a3_string_clone(A3_CS("(untypted)"));

    return dump_get_type(obj->type);
}

static bool dump_bin_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    char* op_str = NULL;
    switch (op->type) {
    case OP_ADD:
        op_str = "+";
        break;
    case OP_ASSIGN:
        op_str = "=";
        break;
    case OP_DIV:
        op_str = "/";
        break;
    case OP_EQ:
        op_str = "==";
        break;
    case OP_GE:
        op_str = ">=";
        break;
    case OP_GT:
        op_str = ">";
        break;
    case OP_LE:
        op_str = "<=";
        break;
    case OP_LT:
        op_str = "<";
        break;
    case OP_MUL:
        op_str = "*";
        break;
    case OP_NE:
        op_str = "!=";
        break;
    case OP_SUB:
        op_str = "-";
        break;
    case OP_OR:
        op_str = "||";
        break;
    case OP_AND:
        op_str = "&&";
        break;
    case OP_MOD:
        op_str = "%";
        break;
    }

    A3String type = dump_get_type(EXPR(op, bin_op)->res_type);
    dump_print(visitor->ctx, "BIN_OP<" A3_S_F ">(%s)", A3_S_FORMAT(type), op_str);
    a3_string_free(&type);

    A3_TRYB(dump_child(visitor, VERTEX(op->lhs, expr)));
    return dump_child(visitor, VERTEX(op->rhs, expr));
}

static bool dump_unary_op(AstVisitor* visitor, UnaryOp* op) {
    assert(visitor);
    assert(op);

    char* op_str = NULL;
    switch (op->type) {
    case OP_UNARY_ADD:
        op_str = "+";
        break;
    case OP_NEG:
        op_str = "-";
        break;
    case OP_ADDR:
        op_str = "&";
        break;
    case OP_DEREF:
        op_str = "*";
        break;
    case OP_NOT:
        op_str = "!";
        break;
    case OP_BW_NOT:
        op_str = "~";
        break;
    case OP_SIZEOF:
        op_str = "sizeof";
        break;
    }

    A3String type = dump_get_type(EXPR(op, unary_op)->res_type);
    dump_print(visitor->ctx, "UNARY_OP<" A3_S_F ">(%s)", A3_S_FORMAT(type), op_str);
    a3_string_free(&type);

    return dump_child(visitor, VERTEX(op->operand, expr));
}

static bool dump_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);
    assert(lit);

    switch (lit->type) {
    case LIT_NUM:
        dump_print(visitor->ctx, "LITERAL(%" PRId64 ")", lit->num);
        break;
    case LIT_STR:
        dump_print(visitor->ctx, "LITERAL(\"" A3_S_F "\")", A3_S_FORMAT(lit->str));
        break;
    }

    return true;
}

static bool dump_var(AstVisitor* visitor, Var* var) {
    assert(visitor);
    assert(var);

    A3String type = dump_obj_type(var->obj);
    dump_print(visitor->ctx, "VAR<" A3_S_F ">(" A3_S_F ")", A3_S_FORMAT(type),
               A3_S_FORMAT(var->name));

    a3_string_free(&type);
    return true;
}

static bool dump_call(AstVisitor* visitor, Call* call) {
    assert(visitor);
    assert(call);

    dump_print(visitor->ctx, "CALL(" A3_S_F ")", A3_S_FORMAT(call->name));

    A3_SLL_FOR_EACH(Arg, arg, &call->args, link) {
        A3_TRYB(dump_child(visitor, VERTEX(arg->expr, expr)));
    }

    return true;
}

static bool dump_member(AstVisitor* visitor, MemberAccess* member) {
    assert(visitor);
    assert(member);

    dump_print(visitor->ctx, "ACCESS(" A3_S_F ")", A3_S_FORMAT(member->rhs->name));
    return dump_child(visitor, VERTEX(member->lhs, expr));
}

static bool dump_expr_cond(AstVisitor* visitor, CondExpr* expr) {
    assert(visitor);
    assert(expr);

    A3String type = dump_get_type(EXPR(expr, cond)->res_type);
    dump_print(visitor->ctx, "COND<" A3_S_F ">", A3_S_FORMAT(type));
    a3_string_free(&type);

    dump_print(visitor->ctx, "IF");
    A3_TRYB(dump_child(visitor, VERTEX(expr->cond, expr)));

    if (expr->res_true) {
        dump_print(visitor->ctx, "THEN");
        A3_TRYB(dump_child(visitor, VERTEX(expr->res_true, expr)));
    } else {
        dump_print(visitor->ctx, "THEN SELF");
    }

    dump_print(visitor->ctx, "ELSE");
    return dump_child(visitor, VERTEX(expr->res_false, expr));
}

static bool dump_expr_type(AstVisitor* visitor, Expr* expr) {
    assert(visitor);
    assert(expr);
    assert(expr->type == EXPR_TYPE);

    A3String type = dump_get_type(expr->res_type);
    dump_print(visitor->ctx, "TY<" A3_S_F ">", A3_S_FORMAT(type));
    a3_string_free(&type);

    return true;
}

static bool dump_expr_stmt(AstVisitor* visitor, Item* stmt) {
    assert(visitor);
    assert(stmt->type == STMT_EXPR_STMT);

    dump_print(visitor->ctx, "EXPR_STMT");

    return dump_child(visitor, VERTEX(stmt->expr, expr));
}

static bool dump_ret(AstVisitor* visitor, Item* ret) {
    assert(visitor);
    assert(ret->type == STMT_RET);

    dump_print(visitor->ctx, "RET");

    return dump_child(visitor, VERTEX(ret->expr, expr));
}

static bool dump_if(AstVisitor* visitor, If* if_stmt) {
    assert(visitor);
    assert(if_stmt);

    dump_print(visitor->ctx, "IF");
    A3_TRYB(dump_child(visitor, VERTEX(if_stmt->cond, expr)));
    dump_print(visitor->ctx, "THEN");
    A3_TRYB(dump_child(visitor, VERTEX(if_stmt->body_true, item)));
    if (if_stmt->body_false) {
        dump_print(visitor->ctx, "ELSE");
        A3_TRYB(dump_child(visitor, VERTEX(if_stmt->body_false, item)));
    }

    return true;
}

static bool dump_block(AstVisitor* visitor, Block* block) {
    assert(visitor);
    assert(block);

    dump_print(visitor->ctx, "BLOCK");

    A3_SLL_FOR_EACH(Item, item, &block->body, link) {
        A3_TRYB(dump_child(visitor, VERTEX(item, item)));
    }

    return true;
}

static bool dump_loop(AstVisitor* visitor, Loop* loop) {
    assert(visitor);
    assert(loop);

    if (loop->init)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->init, item)));

    dump_print(visitor->ctx, "LOOP");

    if (loop->cond) {
        dump_print(visitor->ctx, "WHILE");
        A3_TRYB(dump_child(visitor, VERTEX(loop->cond, expr)));
    } else {
        dump_print(visitor->ctx, "FOREVER");
    }

    dump_print(visitor->ctx, "DO");
    A3_TRYB(dump_child(visitor, VERTEX(loop->body, item)));
    if (loop->post)
        A3_TRYB(dump_child(visitor, VERTEX(loop->post, expr)));

    return true;
}

static bool dump_fn(AstVisitor* visitor, Item* fn) {
    assert(visitor);
    assert(fn);
    assert(fn->decl_type->type == TY_FN);

    A3String type = dump_obj_type(fn->obj);
    dump_print(visitor->ctx, "FN<" A3_S_F ">(" A3_S_F ")", A3_S_FORMAT(type),
               A3_S_FORMAT(fn->name));
    if (fn->body)
        A3_TRYB(dump_child(visitor, VERTEX(fn->body, item.block)));

    a3_string_free(&type);

    return true;
}

static bool dump_decl(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(VERTEX(decl, item)->type == V_DECL);

    if (!decl->name.ptr)
        return true;

    if (decl->decl_type->type == TY_FN)
        return dump_fn(visitor, decl);

    A3String  type = type_name(decl->decl_type);
    A3CString name = type.ptr ? A3_S_CONST(type) : A3_CS("(untyped)");

    dump_print(visitor->ctx, "DECL<" A3_S_F ">(" A3_S_F ")", A3_S_FORMAT(name),
               A3_S_FORMAT(decl->name));

    if (type.ptr)
        a3_string_free(&type);
    return true;
}

bool dump(Vertex* root) {
    assert(root);

    fputs("AST:\n", stderr);

    bool ret = vertex_visit(
        &(AstVisitor) {
            .ctx             = &(Dumper) { .indent = 0 },
            .visit_bin_op    = dump_bin_op,
            .visit_unary_op  = dump_unary_op,
            .visit_lit       = dump_lit,
            .visit_var       = dump_var,
            .visit_call      = dump_call,
            .visit_member    = dump_member,
            .visit_expr_cond = dump_expr_cond,
            .visit_expr_type = dump_expr_type,
            .visit_expr_stmt = dump_expr_stmt,
            .visit_ret       = dump_ret,
            .visit_decl      = dump_decl,
            .visit_if        = dump_if,
            .visit_block     = dump_block,
            .visit_loop      = dump_loop,
        },
        root);

    fputc('\n', stderr);

    return ret;
}

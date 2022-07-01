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
    }

    dump_print(visitor->ctx, "BIN_OP(%s)", op_str);

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
    }

    dump_print(visitor->ctx, "UNARY_OP(%s)", op_str);

    return dump_child(visitor, VERTEX(op->operand, expr));
}

static bool dump_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);
    assert(lit);

    switch (lit->type) {
    case LIT_NUM:
        dump_print(visitor->ctx, "LITERAL(%" PRId64 ")", lit->num);
        break;
    }

    return true;
}

static bool dump_var(AstVisitor* visitor, Var* var) {
    assert(visitor);
    assert(var);

    A3String name = A3_S_NULL;
    if (var->obj && var->obj->type)
        name = type_name(var->obj->type);

    A3CString ty = name.ptr ? A3_S_CONST(name) : A3_CS("(untyped)");
    dump_print(visitor->ctx, "VAR<" A3_S_F ">(" A3_S_F ")", A3_S_FORMAT(ty),
               A3_S_FORMAT(var->name));

    if (name.ptr)
        a3_string_free(&name);

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

static bool dump_decl(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(VERTEX(decl, item)->type == V_DECL);

    A3String  type = type_name(decl->decl_type);
    A3CString name = type.ptr ? A3_S_CONST(type) : A3_CS("(untyped)");

    dump_print(visitor->ctx, "DECL<" A3_S_F ">(" A3_S_F ")", A3_S_FORMAT(name),
               A3_S_FORMAT(decl->name));

    if (type.ptr)
        a3_string_free(&type);
    return true;
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
    dump_print(visitor->ctx, "WHILE");
    A3_TRYB(dump_child(visitor, VERTEX(loop->cond, expr)));
    dump_print(visitor->ctx, "DO");
    A3_TRYB(dump_child(visitor, VERTEX(loop->body, item)));
    if (loop->post)
        A3_TRYB(dump_child(visitor, VERTEX(loop->post, expr)));

    return true;
}

static bool dump_fn(AstVisitor* visitor, Fn* fn) {
    assert(visitor);
    assert(fn);
    assert(fn->type->type == TY_FN);

    A3String  fn_type   = type_name(fn->type);
    A3CString type_name = fn_type.ptr ? A3_S_CONST(fn_type) : A3_CS("(untyped)");

    dump_print(visitor->ctx, "FN<" A3_S_F ">(" A3_S_F ")", A3_S_FORMAT(type_name),
               A3_S_FORMAT(fn->name));
    A3_TRYB(dump_child(visitor, VERTEX(fn->body, item.block)));

    if (fn_type.ptr)
        a3_string_free(&fn_type);

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
            .visit_expr_stmt = dump_expr_stmt,
            .visit_ret       = dump_ret,
            .visit_decl      = dump_decl,
            .visit_if        = dump_if,
            .visit_block     = dump_block,
            .visit_loop      = dump_loop,
            .visit_fn        = dump_fn,
        },
        root);

    fputc('\n', stderr);

    return ret;
}
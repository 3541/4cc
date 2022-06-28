/*
 * GEN -- x86_64 Codegen.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 *
 * Emits nasm-style assembly.
 */

#include "gen.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include <a3/str.h>
#include <a3/util.h>

#include "ast.h"
#include "error.h"

typedef struct Generator {
    A3CString src;
    size_t    stack_depth;
    size_t    label;
} Generator;

static size_t gen_label(Generator* gen) {
    assert(gen);

    return gen->label++;
}

A3_FORMAT_FN(3, 4)
static void gen_error(Generator* gen, Vertex* vertex, char* fmt, ...) {
    assert(gen);
    assert(vertex);

    va_list args;
    va_start(args, fmt);

    verror_at(gen->src, vertex->span, fmt, args);

    va_end(args);
}

static void gen_stack_push(Generator* gen) {
    assert(gen);
    assert(gen);

    puts("push rax");
    gen->stack_depth++;
}

static void gen_stack_pop(Generator* gen, char* reg) {
    assert(gen);
    assert(gen->stack_depth);

    printf("pop %s\n", reg);
    gen->stack_depth--;
}

static bool gen_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);
    assert(lit->type == LIT_NUM);

    printf("mov rax, %" PRId64 "\n", lit->num);
    return true;
}

static bool gen_addr(AstVisitor* visitor, Expr* lvalue) {
    assert(visitor);
    assert(lvalue);

    switch (lvalue->type) {
    case EXPR_VAR:
        printf("lea rax, [rbp - %zu]\n", lvalue->var->stack_offset);
        break;
    case EXPR_UNARY_OP:
        if (lvalue->unary_op.type == OP_DEREF) {
            vertex_visit(visitor, VERTEX(lvalue->unary_op.operand, expr));
            break;
        }

        // fallthrough
    default:
        gen_error(visitor->ctx, VERTEX(lvalue, expr), "Expected an lvalue.");
        return false;
    }

    return true;
}

static bool gen_assign(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);
    assert(op->type == OP_ASSIGN);

    A3_TRYB(gen_addr(visitor, op->lhs));
    gen_stack_push(visitor->ctx);
    vertex_visit(visitor, VERTEX(op->rhs, expr));
    gen_stack_pop(visitor->ctx, "rdi");

    puts("mov [rdi], rax");

    return true;
}

static bool gen_bin_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    if (op->type == OP_ASSIGN)
        return gen_assign(visitor, op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->rhs, expr)));
    gen_stack_push(visitor->ctx);
    A3_TRYB(vertex_visit(visitor, VERTEX(op->lhs, expr)));
    gen_stack_pop(visitor->ctx, "rdi");

    // Arguments now in rdi, rax.

    switch (op->type) {
    case OP_ADD:
        puts("add rax, rdi");
        break;
    case OP_SUB:
        puts("sub rax, rdi");
        break;
    case OP_MUL:
        puts("imul rax, rdi");
        break;
    case OP_DIV:
        puts("cqo\n"
             "idiv rdi");
        break;
    case OP_EQ:
    case OP_NE:
    case OP_LT:
    case OP_LE:
    case OP_GT:
    case OP_GE: {
        puts("cmp rax, rdi");

        char* insn;
        switch (op->type) {
        case OP_EQ:
            insn = "sete";
            break;
        case OP_NE:
            insn = "setne";
            break;
        case OP_LT:
            insn = "setl";
            break;
        case OP_LE:
            insn = "setle";
            break;
        case OP_GT:
            insn = "setg";
            break;
        case OP_GE:
            insn = "setge";
            break;
        default:
            A3_UNREACHABLE();
        }

        printf("%s al\n"
               "movzx rax, al\n",
               insn);
        break;
    }
    case OP_ASSIGN:
        A3_UNREACHABLE();
    }

    return true;
}

static bool gen_unary_op(AstVisitor* visitor, UnaryOp* op) {
    assert(visitor);
    assert(op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->operand, expr)));

    switch (op->type) {
    case OP_UNARY_ADD:
        break;
    case OP_NEG:
        puts("neg rax");
        break;
    case OP_ADDR:
        A3_TRYB(gen_addr(visitor, op->operand));
        break;
    case OP_DEREF:
        A3_TRYB(vertex_visit(visitor, VERTEX(op->operand, expr)));
        puts("mov rax, [rax]");
    }

    return true;
}

static bool gen_ret(AstVisitor* visitor, Statement* ret) {
    assert(visitor);
    assert(ret);
    assert(ret->type == STMT_RET);

    A3_TRYB(vertex_visit(visitor, VERTEX(ret->expr, expr)));
    puts("jmp .ret");
    return true;
}

static bool gen_var(AstVisitor* visitor, Var** var) {
    assert(visitor);
    assert(var);

    A3_TRYB(gen_addr(visitor, EXPR(var, var)));
    puts("mov rax, [rax]");

    return true;
}

static size_t align_up(size_t n, size_t align) { return (n + align - 1) & ~(align - 1); }

static bool gen_fn(AstVisitor* visitor, Fn* fn) {
    assert(visitor);
    assert(fn);

    Block* body              = fn->body;
    body->scope->stack_depth = align_up(body->scope->stack_depth, 16);

    printf("global " A3_S_F "\n"
           "section .text\n"
           "" A3_S_F ":\n"
           "push rbp\n"
           "mov rbp, rsp\n"
           "sub rsp, %zu\n",
           A3_S_FORMAT(fn->name), A3_S_FORMAT(fn->name), body->scope->stack_depth);

    A3_TRYB(vertex_visit(visitor, VERTEX(fn->body, stmt.block)));

    puts(".ret:\n"
         "mov rsp, rbp\n"
         "pop rbp\n"
         "ret");

    return true;
}

static bool gen_if(AstVisitor* visitor, If* if_stmt) {
    assert(visitor);
    assert(if_stmt);

    size_t label = gen_label(visitor->ctx);

    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->cond, expr)));
    printf("test rax, rax\n"
           "jz .else%zu\n",
           label);
    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_true, stmt)));
    printf("jmp .end%zu\n", label);

    printf(".else%zu:\n", label);
    if (if_stmt->body_false)
        A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_false, stmt)));

    printf(".end%zu:\n", label);
    return true;
}

static bool gen_loop(AstVisitor* visitor, Loop* loop) {
    assert(visitor);
    assert(loop);

    size_t label = gen_label(visitor->ctx);

    if (loop->init)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->init, stmt)));

    printf(".begin%zu:\n", label);
    if (loop->cond) {
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->cond, expr)));
        printf("test rax, rax\n"
               "jz .end%zu\n",
               label);
    }

    A3_TRYB(vertex_visit(visitor, VERTEX(loop->body, stmt)));
    if (loop->post)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->post, expr)));

    printf("jmp .begin%zu\n"
           ".end%zu:\n",
           label, label);

    return true;
}

bool gen(A3CString src, Vertex* root) {
    assert(src.ptr);
    assert(root);
    assert(root->type == V_FN);

    Generator gen = { .src = src, .stack_depth = 0, .label = 0 };

    bool ret = vertex_visit(
        &(AstVisitor) {
            .ctx             = &gen,
            .visit_bin_op    = gen_bin_op,
            .visit_unary_op  = gen_unary_op,
            .visit_lit       = gen_lit,
            .visit_var       = gen_var,
            .visit_expr_stmt = NULL,
            .visit_ret       = gen_ret,
            .visit_if        = gen_if,
            .visit_block     = NULL,
            .visit_fn        = gen_fn,
            .visit_loop      = gen_loop,
        },
        root);
    assert(!gen.stack_depth);

    return ret;
}

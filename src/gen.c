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
} Generator;

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

static bool gen_addr(Generator* gen, Var* lvalue) {
    assert(gen);
    assert(lvalue);

    printf("lea rax, [rbp - %zu]\n", lvalue->stack_offset);

    return true;
}

static bool gen_assign(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);
    assert(op->type == OP_ASSIGN);
    assert(op->lhs->type == V_VAR);

    A3_TRYB(gen_addr(visitor->ctx, op->lhs->var));
    gen_stack_push(visitor->ctx);
    vertex_visit(visitor, op->rhs);
    gen_stack_pop(visitor->ctx, "rdi");

    puts("mov [rdi], rax");

    return true;
}

static bool gen_bin_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    if (op->type == OP_ASSIGN)
        return gen_assign(visitor, op);

    A3_TRYB(vertex_visit(visitor, op->rhs));
    gen_stack_push(visitor->ctx);
    A3_TRYB(vertex_visit(visitor, op->lhs));
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

    A3_TRYB(vertex_visit(visitor, op->operand));

    switch (op->type) {
    case OP_UNARY_ADD:
        break;
    case OP_NEG:
        puts("neg rax");
        break;
    }

    return true;
}

static bool gen_expr_stmt(AstVisitor* visitor, Statement* stmt) {
    assert(visitor);
    assert(stmt);
    assert(stmt->type == STMT_EXPR_STMT);

    return vertex_visit(visitor, stmt->expr);
}

static bool gen_ret(AstVisitor* visitor, Statement* ret) {
    assert(visitor);
    assert(ret);
    assert(ret->type == STMT_RET);

    A3_TRYB(vertex_visit(visitor, ret->expr));
    puts("jmp .ret");
    return true;
}

static bool gen_var(AstVisitor* visitor, Var* var) {
    assert(visitor);
    assert(var);

    A3_TRYB(gen_addr(visitor->ctx, var));
    puts("mov rax, [rax]");

    return true;
}

static size_t align_up(size_t n, size_t align) { return (n + align - 1) & ~(align - 1); }

static bool gen_fn(AstVisitor* visitor, Fn* fn) {
    assert(visitor);
    assert(fn);

    Block* body              = &fn->body->stmt.block;
    body->scope->stack_depth = align_up(body->scope->stack_depth, 16);

    printf("global " A3_S_F "\n"
           "section .text\n"
           "" A3_S_F ":\n"
           "push rbp\n"
           "mov rbp, rsp\n"
           "sub rsp, %zu\n",
           A3_S_FORMAT(fn->name), A3_S_FORMAT(fn->name), body->scope->stack_depth);

    A3_TRYB(vertex_visit(visitor, fn->body));

    puts(".ret:\n"
         "mov rsp, rbp\n"
         "pop rbp\n"
         "ret");

    return true;
}

static bool gen_block(AstVisitor* visitor, Block* block) {
    assert(visitor);
    assert(block);

    A3_SLL_FOR_EACH(Vertex, stmt, &block->body, stmt.link) {
        assert(stmt->type == V_STMT);
        A3_TRYB(vertex_visit(visitor, stmt));
    }

    return true;
}

bool gen(A3CString src, Vertex* root) {
    assert(root);
    assert(root->type == V_FN);

    Generator gen = { .src = src, .stack_depth = 0 };

    bool ret = vertex_visit(
        &(AstVisitor) {
            .ctx             = &gen,
            .visit_lit       = gen_lit,
            .visit_bin_op    = gen_bin_op,
            .visit_unary_op  = gen_unary_op,
            .visit_expr_stmt = gen_expr_stmt,
            .visit_ret       = gen_ret,
            .visit_var       = gen_var,
            .visit_fn        = gen_fn,
            .visit_block     = gen_block,
        },
        root);
    assert(!gen.stack_depth);

    return ret;
}

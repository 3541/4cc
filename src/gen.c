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
    bool      status;
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

static void gen_lit(AstVisitor* visitor, Vertex* lit) {
    assert(visitor);
    assert(lit);
    assert(lit->type == V_LIT);

    printf("mov rax, %" PRId64 "\n", lit->lit_num);
}

static void gen_bin_op(AstVisitor* visitor, Vertex* op) {
    assert(visitor);
    assert(op);
    assert(op->type == V_BIN_OP);

    vertex_visit(visitor, op->rhs);
    gen_stack_push(visitor->ctx);
    vertex_visit(visitor, op->lhs);
    gen_stack_pop(visitor->ctx, "rdi");

    // Arguments now in rdi, rax.

    switch (op->bin_op_type) {
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
    default: {
        Generator* gen = visitor->ctx;
        gen->status    = GEN_ERR;

        gen_error(gen, op, "Unrecognized binary operator.");
    }
    }
}

static void gen_unary_op(AstVisitor* visitor, Vertex* op) {
    assert(visitor);
    assert(op);
    assert(op->type == V_UNARY_OP);

    vertex_visit(visitor, op->operand);

    switch (op->unary_op_type) {
    case OP_UNARY_ADD:
        break;
    case OP_NEG:
        puts("neg rax");
        break;
    }
}

bool gen(A3CString src, Vertex* root) {
    Generator gen = { .src = src, .stack_depth = 0, .status = GEN_OK };

    puts("global main\n"
         "section .text\n"
         "main:");

    vertex_visit(
        &(AstVisitor) {
            .ctx            = &gen,
            .visit_lit      = gen_lit,
            .visit_bin_op   = gen_bin_op,
            .visit_unary_op = gen_unary_op,
        },
        root);
    assert(!gen.stack_depth);

    puts("ret");

    return gen.status;
}

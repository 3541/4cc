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
#include "type.h"

static char* REGISTERS_8[]  = { "dil", "sil", "dl", "cl", "r8b", "r9b" };
static char* REGISTERS_64[] = { "rdi", "rsi", "rdx", "rcx", "r8", "r9" };

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

static void gen_load(Type const* type) {
    assert(type);

    if (type->type == TY_ARRAY)
        return;

    puts("mov rax, [rax]");
}

static void gen_store(Generator* gen, Type const* type) {
    assert(gen);

    gen_stack_pop(gen, "rdi");

    if (type_size(type) == 1)
        puts("mov BYTE [rdi], al");
    else
        puts("mov [rdi], rax");
}

static bool gen_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);
    assert(lit->type == LIT_NUM);
    (void)visitor;

    printf("mov rax, %" PRId64 "\n", lit->num);
    return true;
}

static bool gen_addr(AstVisitor* visitor, Expr* lvalue) {
    assert(visitor);
    assert(lvalue);

    switch (lvalue->type) {
    case EXPR_VAR:
        printf("lea rax, [rbp - %zu]\n", lvalue->var.obj->stack_offset);
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
    gen_store(visitor->ctx, EXPR(op, bin_op)->res_type);

    return true;
}

static bool gen_add_sub(BinOp* op) {
    assert(op);
    assert(op->type == OP_ADD || op->type == OP_SUB);

    int count_scalar = type_is_scalar(op->lhs->res_type) + type_is_scalar(op->rhs->res_type);
    assert(op->type == OP_SUB || count_scalar != 0);

    char const* insn = op->type == OP_ADD ? "add" : "sub";

    // Scalar type needs to be multiplied by sizeof(ptr).
    if (count_scalar == 1) {
        char const* scalar_reg = NULL;
        Type const* ptr_type   = NULL;
        if (type_is_scalar(op->lhs->res_type)) {
            scalar_reg = "rax";
            ptr_type   = op->rhs->res_type;
        } else {
            scalar_reg = "rdi";
            ptr_type   = op->lhs->res_type;
        }

        printf("imul %s, %zu\n", scalar_reg, type_size(ptr_type->parent));
    }

    printf("%s rax, rdi\n", insn);

    // Pointer difference is in units of elements.
    if (op->type == OP_SUB && count_scalar == 0) {
        printf("cqo\n"
               "mov rsi, %zu\n"
               "idiv rsi\n",
               type_size(op->lhs->res_type));
    }

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
    case OP_SUB: {
        A3_TRYB(gen_add_sub(op));
        break;
    }
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

    if (op->type == OP_ADDR)
        return gen_addr(visitor, op->operand);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->operand, expr)));

    switch (op->type) {
    case OP_UNARY_ADD:
        break;
    case OP_NEG:
        puts("neg rax");
        break;
    case OP_DEREF:
        gen_load(EXPR(op, unary_op)->res_type);
        break;
    case OP_NOT:
        puts("test rax, rax\n"
             "setz al\n"
             "movzx rax, al");
        break;
    case OP_ADDR:
        // Handled earlier.
        A3_UNREACHABLE();
        break;
    }

    return true;
}

static bool gen_ret(AstVisitor* visitor, Item* ret) {
    assert(visitor);
    assert(ret);
    assert(ret->type == STMT_RET);

    A3_TRYB(vertex_visit(visitor, VERTEX(ret->expr, expr)));
    puts("jmp .ret");
    return true;
}

static bool gen_var(AstVisitor* visitor, Var* var) {
    assert(visitor);
    assert(var);

    A3_TRYB(gen_addr(visitor, EXPR(var, var)));
    gen_load(var->obj->type);

    return true;
}

static bool gen_call(AstVisitor* visitor, Call* call) {
    assert(visitor);
    assert(call);

    size_t args = 0;
    A3_SLL_FOR_EACH(Arg, arg, &call->args, link) {
        args++;
        A3_TRYB(vertex_visit(visitor, VERTEX(arg->expr, expr)));
        gen_stack_push(visitor->ctx);
    }
    assert(args <= 6);

    for (size_t i = 0; i < args; i++)
        gen_stack_pop(visitor->ctx, REGISTERS_64[args - i - 1]);

    printf("extern " A3_S_F "\n"
           "call " A3_S_F "\n",
           A3_S_FORMAT(call->name), A3_S_FORMAT(call->name));
    return true;
}

static bool gen_decl(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(decl);

    if (!decl->obj || decl->obj->type->type != TY_FN)
        return true;

    printf("\nglobal " A3_S_F "\n"
           "" A3_S_F ":\n"
           "push rbp\n"
           "mov rbp, rsp\n"
           "sub rsp, %zu\n",
           A3_S_FORMAT(decl->name), A3_S_FORMAT(decl->name), decl->obj->stack_depth);

    size_t i = 0;
    A3_SLL_FOR_EACH(Item, param, &decl->obj->type->params, link) {
        if (type_size(param->obj->type) == 1)
            printf("movsx BYTE [rbp - %zu], %s\n", param->obj->stack_offset, REGISTERS_8[i++]);
        else
            printf("mov [rbp - %zu], %s\n", param->obj->stack_offset, REGISTERS_64[i++]);
        assert(i <= 6);
    }

    A3_TRYB(vertex_visit(visitor, VERTEX(decl->body, item.block)));
    assert(!((Generator*)visitor->ctx)->stack_depth);

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
    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_true, item)));
    printf("jmp .end%zu\n", label);

    printf(".else%zu:\n", label);
    if (if_stmt->body_false)
        A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_false, item)));

    printf(".end%zu:\n", label);
    return true;
}

static bool gen_loop(AstVisitor* visitor, Loop* loop) {
    assert(visitor);
    assert(loop);

    size_t label = gen_label(visitor->ctx);

    if (loop->init)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->init, item)));

    printf(".begin%zu:\n", label);
    if (loop->cond) {
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->cond, expr)));
        printf("test rax, rax\n"
               "jz .end%zu\n",
               label);
    }

    A3_TRYB(vertex_visit(visitor, VERTEX(loop->body, item)));
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
    assert(root->type == V_UNIT);

    Generator gen = { .src = src, .stack_depth = 0, .label = 0 };

    puts("section .text");

    bool ret = vertex_visit(
        &(AstVisitor) {
            .ctx            = &gen,
            .visit_bin_op   = gen_bin_op,
            .visit_unary_op = gen_unary_op,
            .visit_lit      = gen_lit,
            .visit_var      = gen_var,
            .visit_call     = gen_call,
            .visit_ret      = gen_ret,
            .visit_if       = gen_if,
            .visit_decl     = gen_decl,
            .visit_loop     = gen_loop,
        },
        root);
    assert(!gen.stack_depth);

    return ret;
}

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
#include "config.h"
#include "error.h"
#include "type.h"

static char* REGISTERS_8[]  = { "dil", "sil", "dl", "cl", "r8b", "r9b" };
static char* REGISTERS_64[] = { "rdi", "rsi", "rdx", "rcx", "r8", "r9" };

typedef struct Generator {
    Config const* cfg;
    A3CString     src;
    size_t        stack_depth;
    size_t        label;
    size_t        line;
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

A3_FORMAT_FN(2, 3)
static void gen_asm(Generator* gen, char* fmt, ...) {
    assert(gen);

    va_list args;
    va_start(args, fmt);

    vfprintf(gen->cfg->out, fmt, args);
    fputc('\n', gen->cfg->out);

    va_end(args);
}

static void gen_stack_push(Generator* gen) {
    assert(gen);
    assert(gen);

    gen_asm(gen, "push rax");
    gen->stack_depth++;
}

static void gen_stack_pop(Generator* gen, char* reg) {
    assert(gen);
    assert(gen->stack_depth);

    gen_asm(gen, "pop %s", reg);
    gen->stack_depth--;
}

static void gen_load(Generator* gen, Type const* type) {
    assert(gen);
    assert(type);

    if (type->type == TY_ARRAY)
        return;

    gen_asm(gen, "mov rax, [rax]");
}

static void gen_store(Generator* gen, Type const* type) {
    assert(gen);

    gen_stack_pop(gen, "rdi");

    if (type->size == 1)
        gen_asm(gen, "mov BYTE [rdi], al");
    else
        gen_asm(gen, "mov [rdi], rax");
}

static bool gen_line(AstVisitor* visitor, Vertex* vertex) {
    assert(visitor);
    assert(vertex);

    Generator* gen = visitor->ctx;
    if (gen->line >= vertex->span.line)
        return true;

    gen_asm(gen, "%%line %zu+0 " A3_S_F, vertex->span.line, A3_S_FORMAT(gen->cfg->src));
    gen->line = vertex->span.line;

    return true;
}

static bool gen_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);
    assert(lit->type == LIT_NUM);

    gen_asm(visitor->ctx, "mov rax, %" PRId64, lit->num);
    return true;
}

static bool gen_addr(AstVisitor* visitor, Expr* lvalue) {
    assert(visitor);
    assert(lvalue);

    switch (lvalue->type) {
    case EXPR_VAR:
        if (lvalue->var.obj->global)
            gen_asm(visitor->ctx, "lea rax, [rel " A3_S_F "]", A3_S_FORMAT(lvalue->var.obj->name));
        else
            gen_asm(visitor->ctx, "lea rax, [rbp - %zu]", lvalue->var.obj->stack_offset);
        break;
    case EXPR_MEMBER:
        A3_TRYB(gen_addr(visitor, lvalue->member.lhs));
        gen_asm(visitor->ctx, "add rax, %zu", lvalue->member.rhs->offset);
        break;
    case EXPR_UNARY_OP:
        if (lvalue->unary_op.type == OP_DEREF) {
            A3_TRYB(vertex_visit(visitor, VERTEX(lvalue->unary_op.operand, expr)));
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

static bool gen_add_sub(Generator* gen, BinOp* op) {
    assert(gen);
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

        if (ptr_type->parent->size != 1)
            gen_asm(gen, "imul %s, %zu", scalar_reg, ptr_type->parent->size);
    }

    gen_asm(gen, "%s rax, rdi", insn);

    // Pointer difference is in units of elements.
    if (op->type == OP_SUB && count_scalar == 0 && op->lhs->res_type->parent->size != 1) {
        gen_asm(gen,
                "cqo\n"
                "mov rsi, %zu\n"
                "idiv rsi",
                op->lhs->res_type->parent->size);
    }

    return true;
}

static bool gen_bool_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);
    assert(op->type == OP_OR || op->type == OP_AND);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->lhs, expr)));

    size_t label = gen_label(visitor->ctx);
    gen_asm(visitor->ctx,
            "test rax, rax\n"
            "setnz al\n"
            "movzx rax, al\n"
            "%s .end%zu",
            op->type == OP_AND ? "jz" : "jnz", label);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->rhs, expr)));
    gen_asm(visitor->ctx,
            "test rax, rax\n"
            "setnz al\n"
            "movzx rax, al\n"
            ".end%zu:",
            label);

    return true;
}

static bool gen_member(AstVisitor* visitor, MemberAccess* mem) {
    assert(visitor);
    assert(mem);

    A3_TRYB(gen_addr(visitor, EXPR(mem, member)));
    gen_load(visitor->ctx, EXPR(mem, member)->res_type);

    return true;
}

static bool gen_bin_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    if (op->type == OP_ASSIGN)
        return gen_assign(visitor, op);
    if (op->type == OP_OR || op->type == OP_AND)
        return gen_bool_op(visitor, op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->rhs, expr)));
    gen_stack_push(visitor->ctx);
    A3_TRYB(vertex_visit(visitor, VERTEX(op->lhs, expr)));
    gen_stack_pop(visitor->ctx, "rdi");

    // Arguments now in rdi, rax.

    switch (op->type) {
    case OP_ADD:
    case OP_SUB: {
        A3_TRYB(gen_add_sub(visitor->ctx, op));
        break;
    }
    case OP_MUL:
        gen_asm(visitor->ctx, "imul rax, rdi");
        break;
    case OP_DIV:
        gen_asm(visitor->ctx, "cqo\n"
                              "idiv rdi");
        break;
    case OP_EQ:
    case OP_NE:
    case OP_LT:
    case OP_LE:
    case OP_GT:
    case OP_GE: {
        gen_asm(visitor->ctx, "cmp rax, rdi");

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

        gen_asm(visitor->ctx,
                "%s al\n"
                "movzx rax, al",
                insn);
        break;
    }
    case OP_ASSIGN:
    case OP_OR:
    case OP_AND:
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
        gen_asm(visitor->ctx, "neg rax");
        break;
    case OP_DEREF:
        gen_load(visitor->ctx, EXPR(op, unary_op)->res_type);
        break;
    case OP_NOT:
        gen_asm(visitor->ctx, "test rax, rax\n"
                              "setz al\n"
                              "movzx rax, al");
        break;
    case OP_BW_NOT:
        gen_asm(visitor->ctx, "not rax");
        break;
    case OP_SIZEOF:
        gen_asm(visitor->ctx, "mov rax, %zu\n", op->operand->res_type->size);
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
    gen_asm(visitor->ctx, "jmp .ret");
    return true;
}

static bool gen_var(AstVisitor* visitor, Var* var) {
    assert(visitor);
    assert(var);

    A3_TRYB(gen_addr(visitor, EXPR(var, var)));
    gen_load(visitor->ctx, var->obj->type);

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

    if (!call->obj)
        gen_asm(visitor->ctx, "extern " A3_S_F, A3_S_FORMAT(call->name));
    gen_asm(visitor->ctx, "call " A3_S_F, A3_S_FORMAT(call->name));

    return true;
}

static bool gen_decl(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(decl);

    if (!decl->name.ptr || !decl->obj || decl->obj->type->type != TY_FN)
        return true;

    gen_asm(visitor->ctx,
            "\nglobal " A3_S_F "\n"
            "" A3_S_F ":\n"
            "push rbp\n"
            "mov rbp, rsp\n"
            "sub rsp, %zu",
            A3_S_FORMAT(decl->name), A3_S_FORMAT(decl->name), decl->obj->stack_depth);

    size_t i = 0;
    A3_SLL_FOR_EACH(Item, param, &decl->obj->type->params, link) {
        if (param->obj->type->size == 1)
            gen_asm(visitor->ctx, "movsx BYTE [rbp - %zu], %s", param->obj->stack_offset,
                    REGISTERS_8[i++]);
        else
            gen_asm(visitor->ctx, "mov [rbp - %zu], %s", param->obj->stack_offset,
                    REGISTERS_64[i++]);
        assert(i <= 6);
    }

    A3_TRYB(vertex_visit(visitor, VERTEX(decl->body, item.block)));
    assert(!((Generator*)visitor->ctx)->stack_depth);

    gen_asm(visitor->ctx, "mov rax, 0\n"
                          ".ret:\n"
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
    gen_asm(visitor->ctx,
            "test rax, rax\n"
            "jz .else%zu",
            label);
    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_true, item)));
    gen_asm(visitor->ctx,
            "jmp .end%zu\n"
            ".else%zu:",
            label, label);

    if (if_stmt->body_false)
        A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_false, item)));

    gen_asm(visitor->ctx, ".end%zu:", label);
    return true;
}

static bool gen_loop(AstVisitor* visitor, Loop* loop) {
    assert(visitor);
    assert(loop);

    size_t label = gen_label(visitor->ctx);

    if (loop->init)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->init, item)));

    gen_asm(visitor->ctx, ".begin%zu:", label);
    if (loop->cond) {
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->cond, expr)));
        gen_asm(visitor->ctx,
                "test rax, rax\n"
                "jz .end%zu",
                label);
    }

    A3_TRYB(vertex_visit(visitor, VERTEX(loop->body, item)));
    if (loop->post)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->post, expr)));

    gen_asm(visitor->ctx,
            "jmp .begin%zu\n"
            ".end%zu:",
            label, label);

    return true;
}

bool gen(Config const* cfg, A3CString src, Vertex* root) {
    assert(cfg);
    assert(src.ptr);
    assert(root);
    assert(root->type == V_UNIT);

    Generator gen = { .cfg = cfg, .src = src, .stack_depth = 0, .label = 0, .line = 0 };

    gen_asm(&gen, "section .data");
    A3_SLL_FOR_EACH(Item, decl, &root->unit.items, link) {
        assert(VERTEX(decl, item)->type == V_DECL);

        if (!decl->name.ptr || decl->obj->type->type == TY_FN)
            continue;

        gen_asm(&gen, "global " A3_S_F, A3_S_FORMAT(decl->obj->name));
        if (decl->lit_str.ptr) {
            fprintf(gen.cfg->out, A3_S_F ": db ", A3_S_FORMAT(decl->obj->name));

            for (size_t i = 0; i < decl->lit_str.len; i++)
                fprintf(gen.cfg->out, "%d,", decl->lit_str.ptr[i]);
            gen_asm(&gen, "0");
        } else {
            gen_asm(&gen, A3_S_F ": dq 0", A3_S_FORMAT(decl->obj->name));
        }
    }

    gen.line = 0;
    gen_asm(&gen,
            "\nsection .text\n"
            "%%line 0+0 " A3_S_F,
            A3_S_FORMAT(gen.cfg->src));

    bool ret = vertex_visit(
        &(AstVisitor) {
            .ctx            = &gen,
            .pre            = gen_line,
            .visit_bin_op   = gen_bin_op,
            .visit_unary_op = gen_unary_op,
            .visit_lit      = gen_lit,
            .visit_var      = gen_var,
            .visit_call     = gen_call,
            .visit_member   = gen_member,
            .visit_ret      = gen_ret,
            .visit_if       = gen_if,
            .visit_decl     = gen_decl,
            .visit_loop     = gen_loop,
        },
        root);
    assert(!gen.stack_depth);

    return ret;
}

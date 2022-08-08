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
#include "eval.h"
#include "type.h"
#include "util.h"

typedef enum Register { REG_A, REG_DI, REG_SI, REG_D, REG_C, REG_R8, REG_R9, REG_R10 } Register;

static Register const ARG_REGISTERS[] = { REG_DI, REG_SI, REG_D, REG_C, REG_R8, REG_R9 };

static char const* const REGISTERS_8[] = {
    [REG_A] = "al",   [REG_C] = "cl",   [REG_D] = "dl",   [REG_DI] = "dil",
    [REG_SI] = "sil", [REG_R8] = "r8b", [REG_R9] = "r9b", [REG_R10] = "r10b",
};
static char const* const REGISTERS_16[] = {
    [REG_A] = "ax",  [REG_C] = "cx",   [REG_D] = "dx",   [REG_DI] = "di",
    [REG_SI] = "si", [REG_R8] = "r8w", [REG_R9] = "r9w", [REG_R10] = "r10w",
};
static char const* const REGISTERS_32[] = {
    [REG_A] = "eax",  [REG_C] = "ecx",  [REG_D] = "edx",  [REG_DI] = "edi",
    [REG_SI] = "esi", [REG_R8] = "r8d", [REG_R9] = "r9d", [REG_R10] = "r10d",
};
static char const* const REGISTERS_64[] = {
    [REG_A] = "rax",  [REG_C] = "rcx", [REG_D] = "rdx", [REG_DI] = "rdi",
    [REG_SI] = "rsi", [REG_R8] = "r8", [REG_R9] = "r9", [REG_R10] = "r10",
};

static char const* gen_reg_for(Register reg, Type const* type) {
    assert(type);
    assert(type->size <= 8);

    if (type->size == 1)
        return REGISTERS_8[reg];
    if (type->size == 2)
        return REGISTERS_16[reg];
    if (type->size <= 4)
        return REGISTERS_32[reg];
    return REGISTERS_64[reg];
}

typedef struct LoopCtx {
    size_t break_label;
    size_t continue_label;
    bool   in_loop;
} LoopCtx;

typedef struct Generator {
    Config const* cfg;
    File*         file;
    FILE*         out;
    A3CString     src;
    LoopCtx       in_loop;
    Type const*   init_decl_type;
    size_t        stack_depth;
    size_t        label;
    size_t        line;
} Generator;

static bool gen_data_decl(Generator*, Item*);
static bool gen_data_item(Generator*, Type const*, Init*);

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

    vfprintf(gen->out, fmt, args);
    fputc('\n', gen->out);

    va_end(args);
}

A3_FORMAT_FN(2, 3)
static void gen_asm_nolf(Generator* gen, char* fmt, ...) {
    assert(gen);

    va_list args;
    va_start(args, fmt);

    vfprintf(gen->out, fmt, args);

    va_end(args);
}

static void gen_stack_push(Generator* gen) {
    assert(gen);
    assert(gen);

    gen_asm(gen, "push rax");
    gen->stack_depth++;
}

static void gen_stack_pop(Generator* gen, Register reg) {
    assert(gen);
    assert(gen->stack_depth);

    gen_asm(gen, "pop %s", REGISTERS_64[reg]);
    gen->stack_depth--;
}

static LoopCtx gen_loop_push(Generator* gen, size_t label) {
    assert(gen);

    LoopCtx ret              = gen->in_loop;
    gen->in_loop.in_loop     = true;
    gen->in_loop.break_label = gen->in_loop.continue_label = label;
    return ret;
}

static LoopCtx gen_switch_push(Generator* gen, size_t label) {
    assert(gen);

    LoopCtx ret              = gen->in_loop;
    gen->in_loop.in_loop     = true;
    gen->in_loop.break_label = label;
    return ret;
}

static void gen_loop_pop(Generator* gen, LoopCtx old) {
    assert(gen);
    assert(gen->in_loop.in_loop);

    gen->in_loop = old;
}

static void gen_load(Generator* gen, Type const* type) {
    assert(gen);
    assert(type);

    if (type->type == TY_ARRAY || type->type == TY_STRUCT || type->type == TY_UNION ||
        type->type == TY_FN)
        return;

    if (type->type == TY_PTR || !type->is_signed) {
        if (type->size == 1)
            gen_asm(gen, "movzx rax, BYTE [rax]");
        else if (type->size == 2)
            gen_asm(gen, "movzx rax, WORD [rax]");
        else if (type->size <= 4)
            gen_asm(gen, "mov eax, [rax]");
        else
            gen_asm(gen, "mov rax, [rax]");
    } else {
        if (type->size == 1)
            gen_asm(gen, "movsx rax, BYTE [rax]");
        else if (type->size == 2)
            gen_asm(gen, "movsx rax, WORD [rax]");
        else if (type->size <= 4)
            gen_asm(gen, "movsx rax, DWORD [rax]");
        else
            gen_asm(gen, "mov rax, [rax]");
    }
}

static void gen_store(Generator* gen, Type const* type) {
    assert(gen);

    gen_stack_pop(gen, REG_DI);

    switch (type->type) {
    case TY_STRUCT:
    case TY_UNION:
        gen_asm(gen,
                "mov rcx, %zu\n"
                "mov rsi, rax\n"
                "rep movsb",
                type->size);
        break;
    case TY_ARRAY:
        type = BUILTIN_TYPES[TY_USIZE];
        // fallthrough.
    default:
        gen_asm(gen, "mov [rdi], %s", gen_reg_for(REG_A, type));
        break;
    }
}

static void gen_store_local(Generator* gen, Obj* obj, Register reg) {
    assert(gen);
    assert(obj);
    assert(type_is_scalar_value(obj->type));

    gen_asm(gen, "mov [rbp - %zu], %s", obj->stack_offset, gen_reg_for(reg, obj->type));
}

static bool gen_line(AstVisitor* visitor, Vertex* vertex) {
    assert(visitor);
    assert(vertex);

    Generator* gen = visitor->ctx;
    if (gen->line >= vertex->span.line)
        return true;

    gen_asm(gen, "%%line %zu+0 " A3_S_F, vertex->span.line, A3_S_FORMAT(gen->file->path));
    gen->line = vertex->span.line;

    return true;
}

static bool gen_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);

    switch (lit->type) {
    case LIT_NUM:
        gen_asm(visitor->ctx, "mov rax, %" PRId64, lit->num);
        break;
    case LIT_STR:
        assert(lit->storage);
        assert(lit->storage->is_global);

        gen_asm(visitor->ctx, "lea rax, [rel " A3_S_F "]", A3_S_FORMAT(lit->storage->name));
        break;
    }

    return true;
}

static void gen_addr_obj(Generator* gen, Obj* obj) {
    assert(gen);
    assert(obj);

    if (obj->is_global || obj->is_static)
        gen_asm(gen, "lea rax, [rel $%s" A3_S_F "]", !obj->is_global ? "." : "",
                A3_S_FORMAT(obj->name));
    else
        gen_asm(gen, "lea rax, [rbp - %zu]", obj->stack_offset);
}

static bool gen_addr(AstVisitor* visitor, Expr* lvalue) {
    assert(visitor);
    assert(lvalue);

    switch (lvalue->type) {
    case EXPR_VAR:
        gen_addr_obj(visitor->ctx, lvalue->var.obj);
        break;
    case EXPR_MEMBER:
        A3_TRYB(gen_addr(visitor, lvalue->member.lhs));
        if (lvalue->member.rhs->offset)
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

// Assign an expression to an address on the top of the stack.
static bool gen_assign_to(AstVisitor* visitor, Expr* rhs, Type const* type) {
    assert(visitor);
    assert(rhs);
    assert(type);

    A3_TRYB(vertex_visit(visitor, VERTEX(rhs, expr)));
    gen_store(visitor->ctx, type);

    return true;
}

static bool gen_assign(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);
    assert(op->type == OP_ASSIGN);

    A3_TRYB(gen_addr(visitor, op->lhs));
    gen_stack_push(visitor->ctx);
    return gen_assign_to(visitor, op->rhs, EXPR(op, bin_op)->res_type);
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

    size_t label = util_ident();
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

static bool gen_expr_cond(AstVisitor* visitor, CondExpr* expr) {
    assert(visitor);
    assert(expr);

    A3_TRYB(vertex_visit(visitor, VERTEX(expr->cond, expr)));

    size_t label = util_ident();
    gen_asm(visitor->ctx,
            "test rax, rax\n"
            "jz .false%zu",
            label);

    if (expr->res_true)
        A3_TRYB(vertex_visit(visitor, VERTEX(expr->res_true, expr)));

    gen_asm(visitor->ctx,
            "jmp .end_cond%zu\n"
            ".false%zu:",
            label, label);
    A3_TRYB(vertex_visit(visitor, VERTEX(expr->res_false, expr)));

    gen_asm(visitor->ctx, ".end_cond%zu:", label);

    return true;
}

static bool gen_cast(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);
    assert(op->type == OP_CAST);
    assert(op->lhs->type == EXPR_TYPE);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->rhs, expr)));

    static char const* CASTS[][11] = {
        [TY_I8]    = { [TY_U8 ... TY_U64] = "movzx rax, al" },
        [TY_I16]   = { [TY_I8] = "movsx rax, al",

                       [TY_U8]             = "movzx rax, al",
                       [TY_U16 ... TY_U64] = "movzx rax, ax",
        },
        [TY_I32]   = { [TY_I8]  = "movsx rax, al",
                       [TY_I16] = "movsx rax, ax",

                       [TY_U8 ... TY_U16]  = "movzx rax, al",
                       [TY_U32 ... TY_U64] = "mov eax, eax" },
        [TY_I64]   = { [TY_I8]  = "movsx rax, al",
                       [TY_I16] = "movsx rax, ax",
                       [TY_I32] = "movsx rax, eax",

                       [TY_U8]            = "movzx rax, al",
                       [TY_U16]           = "movzx rax, ax",
                       [TY_U32]           = "mov eax, eax", },

        [TY_U8]    = { [TY_I8 ... TY_I64] = "movsx rax, al" },
        [TY_U16]   = { [TY_I8]             = "movsx rax, al",
                       [TY_I16 ... TY_I64] = "movsx rax, ax",

                       [TY_U8] = "movzx rax, al" },
        [TY_U32]   = { [TY_I8]             = "movsx rax, al",
                       [TY_I16]            = "movsx rax, ax",
                       [TY_I32 ... TY_I64] = "movsx rax, eax",

                       [TY_U8]  = "movzx rax, al",
                       [TY_U16] = "movzx rax, ax" },
        [TY_U64]   = { [TY_I8]  = "movsx rax, al",
                       [TY_I16] = "movsx rax, ax",
                       [TY_I32] = "movsx rax, eax",

                       [TY_U8]  = "movzx rax, al",
                       [TY_U16] = "movzx rax, ax",
                       [TY_U32] = "mov eax, eax" },
    };

    TypeType from = type_to_underlying(op->rhs->res_type->type);
    TypeType to   = type_to_underlying(op->lhs->res_type->type);

    char const* insn = CASTS[from][to];
    if (!insn)
        return true;

    gen_asm(visitor->ctx, "%s", insn);
    return true;
}

static bool gen_bin_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    if (op->type == OP_ASSIGN)
        return gen_assign(visitor, op);
    if (op->type == OP_OR || op->type == OP_AND)
        return gen_bool_op(visitor, op);
    if (op->type == OP_CAST)
        return gen_cast(visitor, op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->rhs, expr)));
    gen_stack_push(visitor->ctx);
    A3_TRYB(vertex_visit(visitor, VERTEX(op->lhs, expr)));
    gen_stack_pop(visitor->ctx, REG_DI);

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
    case OP_MOD:
        gen_asm(visitor->ctx, "cqo\n"
                              "idiv rdi");

        if (op->type == OP_MOD)
            gen_asm(visitor->ctx, "mov rax, rdx");
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
    case OP_SHL:
        gen_asm(visitor->ctx, "mov rcx, rdi");
        gen_asm(visitor->ctx, "shl rax, cl");
        break;
    case OP_SHR:
        gen_asm(visitor->ctx, "mov rcx, rdi");
        if (op->rhs->res_type->is_signed)
            gen_asm(visitor->ctx, "sar rax, cl");
        else
            gen_asm(visitor->ctx, "shr rax, cl");
        break;
    case OP_BW_AND:
        gen_asm(visitor->ctx, "and rax, rdi");
        break;
    case OP_BW_OR:
        gen_asm(visitor->ctx, "or rax, rdi");
        break;
    case OP_BW_XOR:
        gen_asm(visitor->ctx, "xor rax, rdi");
        break;
    case OP_AND:
    case OP_ASSIGN:
    case OP_CAST:
    case OP_OR:
        A3_UNREACHABLE();
    }

    return true;
}

static bool gen_unary_op(AstVisitor* visitor, UnaryOp* op) {
    assert(visitor);
    assert(op);

    if (op->type == OP_ADDR)
        return gen_addr(visitor, op->operand);

    if (op->type != OP_SIZEOF)
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
        gen_asm(visitor->ctx, "mov rax, %zu", op->operand->res_type->size);
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

static bool gen_break_continue(AstVisitor* visitor, Item* item) {
    assert(visitor);
    assert(item);
    assert(item->type == STMT_BREAK || item->type == STMT_CONTINUE);

    Generator* gen = visitor->ctx;

    if (!gen->in_loop.in_loop) {
        gen_error(visitor->ctx, VERTEX(item, item), "%s statement used outside of loop body.",
                  item->type == STMT_BREAK ? "break" : "continue");
        return false;
    }

    if (item->type == STMT_BREAK)
        gen_asm(gen, "jmp .end_%zu", gen->in_loop.break_label);
    else
        gen_asm(gen, "jmp .post_%zu", gen->in_loop.continue_label);

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

    Generator* gen = visitor->ctx;

    bool align_stack =
        (gen->stack_depth + (call->arg_count > 6 ? call->arg_count - 6 : 0)) % 2 != 0;
    if (align_stack) {
        gen_asm(gen, "sub rsp, 8");
        gen->stack_depth++;
    }

    A3_LL_FOR_EACH_REV (Arg, arg, &call->args, link) {
        A3_TRYB(vertex_visit(visitor, VERTEX(arg->expr, expr)));
        gen_stack_push(gen);
    }

    bool indirect = call->callee->res_type->type != TY_FN || call->callee->type != EXPR_VAR ||
                    !call->callee->var.obj->is_global;
    Obj* obj = NULL;
    if (indirect) {
        A3_TRYB(vertex_visit(visitor, VERTEX(call->callee, expr)));
        gen_asm(visitor->ctx, "mov r10, rax");
    } else {
        assert(call->callee->type == EXPR_VAR);
        obj = call->callee->var.obj;
    }

    for (size_t i = 0; i < MIN(call->arg_count, 6); i++)
        gen_stack_pop(gen, ARG_REGISTERS[i]);

    if (!indirect) {
        if (!obj->is_defined)
            gen_asm(gen, "extern " A3_S_F, A3_S_FORMAT(obj->name));

        gen_asm(gen, "call " A3_S_F, A3_S_FORMAT(obj->name));
    } else {
        gen_asm(gen, "call r10");
    }

    if (align_stack) {
        gen_asm(gen, "add rsp, 8");
        gen->stack_depth--;
    }
    if (call->arg_count > 6) {
        gen_asm(gen, "add rsp, %zu", (call->arg_count - 6) * 8);
        gen->stack_depth -= call->arg_count - 6;
    }

    Type const* ret = EXPR(call, call)->res_type;
    if (type_is_scalar(ret) && ret->is_signed && ret->size < 8)
        gen_asm(gen, "movsx rax, %s", gen_reg_for(REG_A, ret));

    return true;
}

static bool gen_fn(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(decl);
    assert(decl->obj && decl->obj->type->type == TY_FN);
    assert(decl->name.ptr);

    if (!decl->body)
        return true;

    gen_asm(visitor->ctx, "%c", '\n');
    if (!decl->attributes.is_static)
        gen_asm(visitor->ctx, "global $" A3_S_F, A3_S_FORMAT(decl->name));

    gen_asm(visitor->ctx,
            "$" A3_S_F ":\n"
            "push rbp\n"
            "mov rbp, rsp\n"
            "sub rsp, %zu",
            A3_S_FORMAT(decl->name), decl->obj->stack_depth);

    if (decl->attributes.is_variadic) {
        assert(decl->obj->va);

        size_t reg_params = 0;
        A3_SLL_FOR_EACH (Item, param, &decl->obj->params, link)
            reg_params++;
        reg_params = MIN(6, reg_params);

        ssize_t offset = decl->obj->va->stack_offset;
        assert(offset >= 0);
        gen_asm(visitor->ctx,
                "mov DWORD [rbp - %zd], %zu\n" // .gp_offset
                "mov DWORD [rbp - %zd], 0\n"   // .fp_offset
                "mov QWORD [rbp - %zd], rbp\n" // .overflow_arg_area
                "add QWORD [rbp - %zd], 16\n"  // .overflow_arg_area
                "mov [rbp - %zd], rbp\n"       // .reg_save_area
                "sub QWORD [rbp - %zd], %zd",  // .reg_save_area
                offset, reg_params * 8, offset - 4, offset - 8, offset - 8, offset - 16,
                offset - 16, offset - 24);
        offset -= 24;

        for (size_t i = 0; i < sizeof(ARG_REGISTERS) / sizeof(ARG_REGISTERS[0]); i++) {
            gen_asm(visitor->ctx, "mov [rbp - %zu], %s", offset, REGISTERS_64[ARG_REGISTERS[i]]);
            offset -= 8;
        }
    }

    size_t i = 0;
    A3_SLL_FOR_EACH (Item, param, &decl->obj->params, link) {
        gen_store_local(visitor->ctx, param->obj, ARG_REGISTERS[i++]);

        if (i >= 6)
            break;
    }

    A3_TRYB(vertex_visit(visitor, VERTEX(decl->body, item.block)));

    gen_asm(visitor->ctx, "mov rax, 0\n"
                          ".ret:\n"
                          "mov rsp, rbp\n"
                          "pop rbp\n"
                          "ret");

    return true;
}

static void gen_zero_fill(Generator* gen, size_t n) {
    switch (n) {
    case 1:
        gen_asm(gen, "mov BYTE [rax], 0");
        break;
    case 2:
        gen_asm(gen, "mov WORD [rax], 0");
        break;
    case 4:
        gen_asm(gen, "mov DWORD [rax], 0");
        break;
    case 8:
        gen_asm(gen, "mov QWORD [rax], 0");
        break;
    default:
        gen_asm(gen,
                "mov rdi, rax\n"
                "mov al, 0\n"
                "mov rcx, %zu\n"
                "rep stosb",
                n);
        break;
    }
}

static bool gen_init(AstVisitor* visitor, Init* init) {
    assert(visitor);
    assert(init);

    Generator*  gen       = visitor->ctx;
    Type const* decl_type = gen->init_decl_type;

    switch (init->type) {
    case INIT_EXPR:
        A3_TRYB(gen_assign_to(visitor, init->expr, decl_type));
        break;
    case INIT_LIST:
        switch (decl_type->type) {
        case TY_ARRAY: {
            size_t i = 0;
            A3_SLL_FOR_EACH (Init, elem, &init->list, link) {
                gen_asm(gen, "mov rax, [rsp]");
                if (i)
                    gen_asm(gen, "add rax, %zu", i * decl_type->parent->size);
                i++;
                gen_stack_push(gen);
                gen->init_decl_type = decl_type->parent;
                A3_TRYB(vertex_visit(visitor, VERTEX(elem, init)));
                gen->init_decl_type = decl_type;
            }

            if (i < decl_type->len) {
                gen_asm(gen, "mov rax, [rsp]");
                if (i)
                    gen_asm(gen, "add rax, %zu", i * decl_type->parent->size);
                gen_zero_fill(gen, (decl_type->len - i) * decl_type->parent->size);
            }

            break;
        }
        case TY_STRUCT: {
            Init* elem = A3_SLL_HEAD(&init->list);
            A3_SLL_FOR_EACH (Member, mem, &decl_type->members, link) {
                gen_asm(gen, "mov rax, [rsp]");
                if (mem->offset)
                    gen_asm(gen, "add rax, %zu", mem->offset);

                if (elem) {
                    gen_stack_push(gen);
                    gen->init_decl_type = mem->type;
                    A3_TRYB(vertex_visit(visitor, VERTEX(elem, init)));
                    gen->init_decl_type = decl_type;

                    elem = A3_SLL_NEXT(elem, link);
                } else {
                    gen_zero_fill(gen, mem->type->size);
                }
            }

            break;
        }
        default:
            A3_UNREACHABLE();
        }

        gen_stack_pop(gen, REG_A);
        break;
    }

    return true;
}

static bool gen_decl(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(decl);

    if (!decl->name.ptr || !decl->obj)
        return true;

    if (decl->obj->type->type == TY_FN)
        return gen_fn(visitor, decl);

    if (decl->obj->is_global || !decl->init)
        return true;

    Generator* gen = visitor->ctx;

    if (decl->attributes.is_static)
        return gen_data_decl(gen, decl);

    gen->init_decl_type = decl->decl_type;

    gen_addr_obj(gen, decl->obj);
    gen_stack_push(gen);
    A3_TRYB(vertex_visit(visitor, VERTEX(decl->init, init)));

    gen->init_decl_type = NULL;

    return true;
}

static bool gen_if(AstVisitor* visitor, If* if_stmt) {
    assert(visitor);
    assert(if_stmt);

    size_t label = util_ident();

    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->cond, expr)));
    gen_asm(visitor->ctx,
            "test rax, rax\n"
            "jz .else%zu",
            label);
    A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_true, item)));
    gen_asm(visitor->ctx,
            "jmp .end_if%zu\n"
            ".else%zu:",
            label, label);

    if (if_stmt->body_false)
        A3_TRYB(vertex_visit(visitor, VERTEX(if_stmt->body_false, item)));

    gen_asm(visitor->ctx, ".end_if%zu:", label);
    return true;
}

static bool gen_loop(AstVisitor* visitor, Loop* loop) {
    assert(visitor);
    assert(loop);

    Generator* gen   = visitor->ctx;
    size_t     label = util_ident();

    if (loop->init)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->init, item)));

    gen_asm(gen, ".begin_%zu:", label);
    if (loop->cond && loop->cond_first) {
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->cond, expr)));
        gen_asm(gen,
                "test rax, rax\n"
                "jz .end_%zu",
                label);
    }

    LoopCtx old = gen_loop_push(gen, label);
    A3_TRYB(vertex_visit(visitor, VERTEX(loop->body, item)));
    gen_loop_pop(gen, old);

    gen_asm(gen, ".post_%zu:", label);
    if (loop->post)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->post, expr)));
    if (loop->cond && !loop->cond_first) {
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->cond, expr)));
        gen_asm(gen,
                "test rax, rax\n"
                "jz .end_%zu",
                label);
    }

    gen_asm(gen,
            "jmp .begin_%zu\n"
            ".end_%zu:",
            label, label);

    return true;
}

static bool gen_data_expr(Generator* gen, Type const* type, Expr* expr) {
    assert(gen);
    assert(type);
    assert(expr);

    if (expr->type == EXPR_LIT && expr->lit.type == LIT_STR) {
        assert(type->type == TY_PTR && type->parent->type == TY_U8);

        gen_asm(gen, "dq $" A3_S_F, A3_S_FORMAT(expr->lit.storage->name));
        return true;
    }

    uintmax_t val = 0;
    if (expr->type != EXPR_LIT) {
        EvalResult res = eval(gen->src, expr);
        if (!res.ok)
            return false;

        val = (uintmax_t)res.value;
    } else {
        assert(expr->lit.type == LIT_NUM);
        val = expr->lit.num;
    }

    char* size = NULL;

    if (type->size == 1)
        size = "db";
    else if (type->size == 2)
        size = "dw";
    else if (type->size <= 4)
        size = "dd";
    else
        size = "dq";

    gen_asm(gen, "%s %" PRIuMAX, size, val);

    return true;
}

static void gen_zeros(Generator* gen, size_t n) {
    assert(gen);

    if (!n)
        return;

    bool first = true;
    gen_asm_nolf(gen, "db ");
    for (size_t i = 0; i < n; i++) {
        gen_asm_nolf(gen, "%s0", !first ? "," : "");
        first = false;
    }
    gen_asm_nolf(gen, "\n");
}

static bool gen_data_list(Generator* gen, Type const* type, Init* list) {
    assert(gen);
    assert(type);
    assert(type->type == TY_ARRAY || type->type == TY_STRUCT);
    assert(list);
    assert(list->type == INIT_LIST);

    if (type->type == TY_ARRAY) {
        size_t count = 0;

        A3_SLL_FOR_EACH (Init, elem, &list->list, link) {
            A3_TRYB(gen_data_item(gen, type->parent, elem));
            count++;
        }

        if (count < type->len)
            gen_zeros(gen, type->size - type->parent->size * count);
    } else {
        Init*  elem = A3_SLL_HEAD(&list->list);
        size_t i    = 0;
        A3_SLL_FOR_EACH (Member, mem, &type->members, link) {
            gen_zeros(gen, mem->offset - i);
            i = mem->offset;

            A3_TRYB(gen_data_item(gen, mem->type, elem));
            elem = A3_SLL_NEXT(elem, link);
            i += mem->type->size;
        }
    }

    return true;
}

static bool gen_data_item(Generator* gen, Type const* type, Init* init) {
    assert(gen);
    assert(type);
    assert(init);

    switch (init->type) {
    case INIT_EXPR:
        return gen_data_expr(gen, type, init->expr);
    case INIT_LIST:
        return gen_data_list(gen, type, init);
    }

    A3_UNREACHABLE();
}

static bool gen_data_decl(Generator* gen, Item* decl) {
    assert(gen);
    assert(decl);
    assert(VERTEX(decl, item)->type == V_DECL);
    assert(decl->obj->is_global || decl->attributes.is_static);

    if (decl->obj->is_defined)
        return true;

    if (decl->attributes.is_extern) {
        gen_asm(gen, "extern $" A3_S_F, A3_S_FORMAT(decl->name));
        return true;
    }

    Type const* type = decl->decl_type;

    if (decl->obj->is_global && !decl->attributes.is_static)
        gen_asm(gen, "global $" A3_S_F, A3_S_FORMAT(decl->name));
    if (!decl->obj->is_global)
        gen_asm(gen, "section .data");
    if (type->align != 1)
        gen_asm(gen, "align %zu, db 0", type->align);

    gen_asm(gen, "$%s" A3_S_F ":", !decl->obj->is_global ? "." : "", A3_S_FORMAT(decl->name));
    if (!decl->obj->init)
        gen_asm(gen, "times %zu db 0", type->size);
    else
        gen_data_item(gen, decl->obj->type, decl->obj->init);
    decl->obj->is_defined = true;

    if (!decl->obj->is_global)
        gen_asm(gen, "section .text");

    return true;
}

static bool gen_label(AstVisitor* visitor, Label* label) {
    assert(visitor);
    assert(label);

    if (!label->is_switch_label)
        gen_asm(visitor->ctx, "$.label_" A3_S_F "%zu:", A3_S_FORMAT(label->name), label->label);
    else if (label->expr)
        gen_asm(visitor->ctx, "$.switch_label_%zu:", label->label);
    else
        gen_asm(visitor->ctx, "$.switch_default_%zu:", label->label);

    return true;
}

static bool gen_goto(AstVisitor* visitor, Goto* jmp) {
    assert(visitor);
    assert(jmp);

    gen_asm(visitor->ctx, "jmp $.label_" A3_S_F "%zu", A3_S_FORMAT(jmp->label), jmp->target->label);
    return true;
}

static bool gen_switch(AstVisitor* visitor, Switch* switch_stmt) {
    assert(visitor);
    assert(switch_stmt);

    A3_TRYB(vertex_visit(visitor, VERTEX(switch_stmt->cond, expr)));

    A3_SLL_FOR_EACH (Label, case_label, &switch_stmt->cases, link) {
        gen_asm(visitor->ctx,
                "cmp rax, %" PRIdMAX "\n"
                "je $.switch_label_%zu",
                case_label->value, case_label->label);
    }

    size_t ident = util_ident();

    if (switch_stmt->default_case)
        gen_asm(visitor->ctx, "jmp $.switch_default_%zu", switch_stmt->default_case->label);
    else
        gen_asm(visitor->ctx, "jmp $.end_%zu", ident);

    LoopCtx old = gen_switch_push(visitor->ctx, ident);

    A3_TRYB(vertex_visit(visitor, VERTEX(switch_stmt->body, item)));

    gen_loop_pop(visitor->ctx, old);

    gen_asm(visitor->ctx, ".end_%zu:", ident);

    return true;
}

static bool gen_data(Generator* gen, Vertex* root) {
    assert(gen);
    assert(root);

    assert(root->type == V_UNIT);

    gen_asm(gen, "section .data");
    A3_SLL_FOR_EACH (Item, decl, &root->unit.items, link) {
        if (decl->decl_type->type == TY_FN || !decl->name.ptr || !decl->obj)
            continue;

        A3_TRYB(gen_data_decl(gen, decl));
    }

    return true;
}

bool gen(Config const* cfg, File* file, A3CString src, A3CString dst, Vertex* root) {
    assert(cfg);
    assert(file);
    assert(src.ptr);
    assert(dst.ptr);
    assert(root);
    assert(root->type == V_UNIT);

    Generator gen = { .cfg            = cfg,
                      .file           = file,
                      .out            = fopen(a3_string_cstr(dst), "w"),
                      .src            = src,
                      .init_decl_type = NULL,
                      .stack_depth    = 0,
                      .label          = 0,
                      .line           = 0,
                      .in_loop        = { .in_loop = false } };

    if (!gen.out) {
        fprintf(stderr, "Failed to open output file " A3_S_F ".\n", A3_S_FORMAT(dst));
        return GEN_ERR;
    }

    A3_TRYB(gen_data(&gen, root));

    gen.line = 0;
    gen_asm(&gen,
            "\nsection .text\n"
            "%%line 0+0 " A3_S_F,
            A3_S_FORMAT(gen.file->path));

    bool ret = vertex_visit(
        &(AstVisitor) {
            .ctx                  = &gen,
            .pre                  = gen_line,
            .visit_bin_op         = gen_bin_op,
            .visit_unary_op       = gen_unary_op,
            .visit_lit            = gen_lit,
            .visit_var            = gen_var,
            .visit_call           = gen_call,
            .visit_member         = gen_member,
            .visit_expr_cond      = gen_expr_cond,
            .visit_break_continue = gen_break_continue,
            .visit_ret            = gen_ret,
            .visit_if             = gen_if,
            .visit_decl           = gen_decl,
            .visit_init           = gen_init,
            .visit_loop           = gen_loop,
            .visit_label          = gen_label,
            .visit_goto           = gen_goto,
            .visit_switch         = gen_switch,
        },
        root);
    if (ret)
        assert(!gen.stack_depth);

    fclose(gen.out);

    return ret;
}

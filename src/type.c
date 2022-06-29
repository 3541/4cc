/*
 * TYPE -- Types.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include "type.h"

#include <assert.h>
#include <stdbool.h>

#include <a3/ht.h>
#include <a3/util.h>

#include "ast.h"
#include "error.h"

static int8_t key_eq(Type const* lhs, Type const* rhs) { return lhs == rhs ? 0 : 1; }
#define KEY_BYTES(K) ((uint8_t const*)(K))
#define KEY_SIZE(K)  (sizeof(Type*))

typedef Type const* TypePtr;
A3_HT_DEFINE_STRUCTS(TypePtr, TypePtr);
A3_HT_DECLARE_METHODS(TypePtr, TypePtr);
A3_HT_DEFINE_METHODS(TypePtr, TypePtr, KEY_BYTES, KEY_SIZE, key_eq)

A3_HT_DEFINE_STRUCTS(A3CString, Obj)

A3_HT_DECLARE_METHODS(A3CString, Obj)
A3_HT_DEFINE_METHODS(A3CString, Obj, a3_string_cptr, a3_string_len, a3_string_cmp)

typedef struct Scope {
    Scope* parent;
    A3_HT(A3CString, Obj) scope;
    size_t stack_depth;
} Scope;

typedef struct Registry {
    A3_HT(TypePtr, TypePtr) ptrs_to;
    Scope*    current_scope;
    A3CString src;
} Registry;

Type const* BUILTIN_TYPES[] = { [TY_INT] = &(Type) { .type = TY_INT } };

static Scope* scope_new(Scope* parent) {
    A3_UNWRAPNI(Scope*, ret, calloc(1, sizeof(*ret)));
    *ret = (Scope) { .parent = parent, .stack_depth = parent ? parent->stack_depth : 0 };
    A3_HT_INIT(A3CString, Obj)(&ret->scope, A3_HT_NO_HASH_KEY, A3_HT_ALLOW_GROWTH);

    return ret;
}

static Obj* scope_find(Scope* scope, A3CString name) {
    assert(scope);
    assert(name.ptr);

    Obj* ret = A3_HT_FIND(A3CString, Obj)(&scope->scope, name);
    if (!ret && scope->parent)
        return scope_find(scope->parent, name);

    return ret;
}

static Obj* scope_add(Scope* scope, Obj obj) {
    assert(scope);
    assert(obj.name.ptr);

    bool res = A3_HT_INSERT(A3CString, Obj)(&scope->scope, obj.name, obj);
    assert(res);
    (void)res;

    return A3_HT_FIND(A3CString, Obj)(&scope->scope, obj.name);
}

size_t scope_stack_depth(Scope* scope) {
    assert(scope);

    return scope->stack_depth;
}

static void reg_scope_push(Registry* reg) {
    assert(reg);

    reg->current_scope = scope_new(reg->current_scope);
}

static void reg_scope_pop(Registry* reg) {
    assert(reg);
    assert(reg->current_scope);

    reg->current_scope = reg->current_scope->parent;
}

Registry* type_registry_new(void) {
    A3_UNWRAPNI(Registry*, ret, calloc(1, sizeof(*ret)));
    A3_HT_INIT(TypePtr, TypePtr)(&ret->ptrs_to, A3_HT_NO_HASH_KEY, A3_HT_ALLOW_GROWTH);

    return ret;
}

A3String type_name(Type const* type) {
    assert(type);

    switch (type->type) {
    case TY_INT:
        return a3_string_clone(A3_CS("int"));
    case TY_PTR: {
        A3String base = type_name(type->parent);
        A3String ret  = a3_string_alloc(base.len + 1);
        a3_string_copy(ret, A3_S_CONST(base));
        ret.ptr[base.len] = '*';
        a3_string_free(&base);
        return ret;
    }
    }

    A3_UNREACHABLE();
}

bool type_is_scalar(Type const* type) {
    assert(type);

    return type->type == TY_INT;
}

size_t type_size(Type const* type) {
    assert(type);

    switch (type->type) {
    case TY_INT:
        return sizeof(int64_t);
    case TY_PTR:
        return sizeof(void*);
    }

    A3_UNREACHABLE();
}

A3_FORMAT_FN(3, 4)
static void type_error(Registry const* reg, Vertex const* vertex, char* fmt, ...) {
    assert(reg);
    assert(vertex);

    va_list args;
    va_start(args, fmt);

    verror_at(reg->src, vertex->span, fmt, args);

    va_end(args);
}

static void type_error_mismatch(Registry const* reg, Vertex const* v, Type const* lhs,
                                Type const* rhs) {
    assert(reg);
    assert(v);
    assert(lhs);
    assert(rhs);

    A3String lhs_name = type_name(lhs);
    A3String rhs_name = type_name(rhs);

    type_error(reg, v, "Types " A3_S_F " and " A3_S_F " are incompatible.", A3_S_FORMAT(lhs_name),
               A3_S_FORMAT(rhs_name));

    a3_string_free(&lhs_name);
    a3_string_free(&rhs_name);
}

static Type const* type_ptr_to(Registry* reg, Type const* type) {
    assert(reg);
    assert(type);

    Type const** entry = A3_HT_FIND(TypePtr, TypePtr)(&reg->ptrs_to, type);
    if (entry)
        return *entry;

    A3_UNWRAPNI(Type*, ret, calloc(1, sizeof(*ret)));
    *ret = (Type) { .type = TY_PTR, .parent = type };

    A3_HT_INSERT(TypePtr, TypePtr)(&reg->ptrs_to, type, ret);

    return ret;
}

static bool type_binary_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->lhs, expr)));
    A3_TRYB(vertex_visit(visitor, VERTEX(op->rhs, expr)));

    // TODO: Check compatibility.
    /*    */

    switch (op->type) {
    case OP_DIV:
    case OP_MUL:
        if (op->lhs->res_type != op->rhs->res_type) {
            type_error_mismatch(visitor->ctx, VERTEX(op, expr.bin_op), op->lhs->res_type,
                                op->rhs->res_type);
            return false;
        }
        if (!type_is_scalar(op->lhs->res_type)) {
            A3String name = type_name(op->lhs->res_type);
            type_error(visitor->ctx, VERTEX(op->lhs, expr),
                       "Non-scalar type" A3_S_F " invalid for this operation.", A3_S_FORMAT(name));
            a3_string_free(&name);
            return false;
        }
        if (!type_is_scalar(op->rhs->res_type)) {
            A3String name = type_name(op->rhs->res_type);
            type_error(visitor->ctx, VERTEX(op->rhs, expr),
                       "Non-scalar type" A3_S_F " invalid for this operation.", A3_S_FORMAT(name));
            a3_string_free(&name);
            return false;
        }

        // fallthrough
    case OP_ADD:
        if (!type_is_scalar(op->lhs->res_type) && !type_is_scalar(op->rhs->res_type)) {
            type_error_mismatch(visitor->ctx, VERTEX(op, expr.bin_op), op->lhs->res_type,
                                op->rhs->res_type);
            return false;
        }

        // fallthrough
    case OP_SUB:
    case OP_ASSIGN:
        // TODO: Typechecking for assignment.
        EXPR(op, bin_op)->res_type = op->lhs->res_type;
        break;
    case OP_EQ:
    case OP_GE:
    case OP_GT:
    case OP_LE:
    case OP_LT:
    case OP_NE:
        EXPR(op, bin_op)->res_type = BUILTIN_TYPES[TY_INT];
        break;
    }

    return true;
}

static bool type_unary_op(AstVisitor* visitor, UnaryOp* op) {
    assert(visitor);
    assert(op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->operand, expr)));

    switch (op->type) {
    case OP_ADDR:
        EXPR(op, unary_op)->res_type = type_ptr_to(visitor->ctx, op->operand->res_type);
        break;
    case OP_DEREF:
        // TODO: Only dereference actual pointers.
        /*        if (op->operand->res_type->type != TY_PTR) {
                    A3String op_name = type_name(op->operand->res_type);
                    type_error(visitor->ctx, VERTEX(op->operand, expr),
                               "Tried to dereference non-pointer type " A3_S_F ".",
           A3_S_FORMAT(op_name)); a3_string_free(&op_name); return false;
                    }*/
        EXPR(op, unary_op)->res_type = op->operand->res_type->parent ?: BUILTIN_TYPES[TY_INT];

        break;
    case OP_NEG:
    case OP_UNARY_ADD:
        EXPR(op, unary_op)->res_type = op->operand->res_type;
        break;
    }

    return true;
}

static bool type_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);
    assert(lit);
    assert(lit->type == LIT_NUM);
    (void)visitor;

    EXPR(lit, lit)->res_type = BUILTIN_TYPES[TY_INT];
    return true;
}

static bool type_var(AstVisitor* visitor, Var* var) {
    assert(visitor);
    assert(var);

    Registry* reg = visitor->ctx;

    if (!reg->current_scope) {
        type_error(reg, VERTEX(var, expr.var), "Variable used oustide scope.");
        return false;
    }

    if (!var->obj) {
        Obj* obj = scope_find(reg->current_scope, var->name);

        if (!obj) {
            obj = scope_add(reg->current_scope,
                            (Obj) { .name         = var->name,
                                    .stack_offset = reg->current_scope->stack_depth,
                                    .type         = BUILTIN_TYPES[TY_INT] });
            reg->current_scope->stack_depth += type_size(obj->type);
        }

        var->obj = obj;
    }

    EXPR(var, var)->res_type = var->obj->type;
    return true;
}

static bool type_block(AstVisitor* visitor, Block* block) {
    assert(visitor);
    assert(block);

    Registry* reg = visitor->ctx;

    reg_scope_push(reg);
    block->scope = reg->current_scope;

    A3_SLL_FOR_EACH(Statement, stmt, &block->body, link) {
        A3_TRYB(vertex_visit(visitor, VERTEX(stmt, stmt)));
    }

    reg_scope_pop(reg);

    return true;
}

static bool type_loop(AstVisitor* visitor, Loop* loop) {
    assert(visitor);
    assert(loop);

    reg_scope_push(visitor->ctx);

    if (loop->init)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->init, stmt)));
    if (loop->cond)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->cond, expr)));
    A3_TRYB(vertex_visit(visitor, VERTEX(loop->body, stmt)));
    if (loop->post)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->post, expr)));

    reg_scope_pop(visitor->ctx);
    return true;
}

static size_t align_up(size_t n, size_t align) { return (n + align - 1) & ~(align - 1); }

static bool type_fn(AstVisitor* visitor, Fn* fn) {
    assert(visitor);
    assert(fn);

    A3_TRYB(vertex_visit(visitor, VERTEX(fn->body, stmt.block)));
    fn->body->scope->stack_depth = align_up(fn->body->scope->stack_depth, 16);

    return true;
}

bool type(Registry* reg, A3CString src, Vertex* root) {
    assert(reg);
    assert(src.ptr);
    assert(root->type == V_FN);

    reg->src = src;

    return vertex_visit(
        &(AstVisitor) {
            .ctx            = reg,
            .visit_bin_op   = type_binary_op,
            .visit_unary_op = type_unary_op,
            .visit_lit      = type_lit,
            .visit_var      = type_var,
            .visit_block    = type_block,
            .visit_loop     = type_loop,
            .visit_fn       = type_fn,
        },
        root);
}

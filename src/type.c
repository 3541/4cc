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
#include <inttypes.h>
#include <stdalign.h>
#include <stdbool.h>

#include <a3/buffer.h>
#include <a3/ht.h>
#include <a3/str.h>
#include <a3/util.h>

#include "ast.h"
#include "error.h"
#include "eval.h"
#include "stdlib.h"

static int8_t key_eq(Type const* lhs, Type const* rhs) { return lhs == rhs ? 0 : 1; }
#define KEY_BYTES(K) ((uint8_t const*)(K))
#define KEY_SIZE(K)  (sizeof(Type*))

typedef Type const* TypePtr;
A3_HT_DEFINE_STRUCTS(TypePtr, TypePtr);
A3_HT_DECLARE_METHODS(TypePtr, TypePtr);
A3_HT_DEFINE_METHODS(TypePtr, TypePtr, KEY_BYTES, KEY_SIZE, key_eq)

A3_HT_DEFINE_STRUCTS(A3CString, TypePtr);
A3_HT_DECLARE_METHODS(A3CString, TypePtr);
A3_HT_DEFINE_METHODS(A3CString, TypePtr, a3_string_cptr, a3_string_len, a3_string_cmp);

typedef Obj* ObjPtr;
A3_HT_DEFINE_STRUCTS(A3CString, ObjPtr)
A3_HT_DECLARE_METHODS(A3CString, ObjPtr)
A3_HT_DEFINE_METHODS(A3CString, ObjPtr, a3_string_cptr, a3_string_len, a3_string_cmp)

typedef struct Scope {
    Scope*                    parent;
    Obj*                      fn;
    A3_HT(A3CString, ObjPtr)  idents;
    A3_HT(A3CString, TypePtr) tags;
    A3_HT(A3CString, TypePtr) typedefs;
} Scope;

typedef struct Registry {
    A3_HT(TypePtr, TypePtr)   ptrs_to;
    A3_HT(A3CString, TypePtr) fns;
    Scope*                    current_scope;
    Unit*                     current_unit;
    A3CString                 src;
    size_t                    lit_count;
    size_t                    init_depth;
    Type const*               init_type;
} Registry;

Type const* BUILTIN_TYPES[] = {
    [TY_VOID] = &(Type) { .type = TY_VOID, .size = 0, .align = 0 },
    [TY_I8]   = &(Type) { .type = TY_I8, .size = 1, .align = 1, .is_signed = true },
    [TY_I16]  = &(Type) { .type = TY_I16, .size = 2, .align = 2, .is_signed = true },
    [TY_I32]  = &(Type) { .type = TY_I32, .size = 4, .align = 4, .is_signed = true },
    [TY_I64]  = &(Type) { .type = TY_I64, .size = 8, .align = 8, .is_signed = true },
    [TY_U8]   = &(Type) { .type = TY_U8, .size = 1, .align = 1, .is_signed = false },
    [TY_U16]  = &(Type) { .type = TY_U16, .size = 2, .align = 2, .is_signed = false },
    [TY_U32]  = &(Type) { .type = TY_U32, .size = 4, .align = 4, .is_signed = false },
    [TY_U64]  = &(Type) { .type = TY_U64, .size = 8, .align = 8, .is_signed = false },
};

static Type const* type_from_ptype(Registry*, PType*);

#define OBJ_GLOBAL true
#define OBJ_LOCAL  false
static Obj* obj_new(A3CString name, Type const* type, Init* init, size_t stack_offset,
                    bool global) {
    assert(name.ptr);
    assert(type);

    A3_UNWRAPNI(Obj*, ret, calloc(1, sizeof(*ret)));
    *ret = (Obj) {
        .name         = name,
        .type         = type,
        .init         = init,
        .stack_offset = stack_offset,
        .is_global    = global,
        .is_defined   = false,
    };

    return ret;
}

#define OBJ_FN_DEFINED   true
#define OBJ_FN_UNDEFINED false
static Obj* obj_fn_new(A3CString name, Type const* type, bool defined) {
    assert(name.ptr);
    assert(type);

    Obj* ret        = obj_new(name, type, NULL, 0, OBJ_GLOBAL);
    ret->is_defined = defined;

    return ret;
}

static Obj* obj_enum_const_new(A3CString name, Type const* type, uint32_t value) {
    assert(name.ptr);
    assert(type);

    Obj* ret              = obj_new(name, type, NULL, 0, OBJ_LOCAL);
    ret->is_named_literal = true;
    ret->value            = value;

    return ret;
}

static Scope* scope_new(Scope* parent) {
    A3_UNWRAPNI(Scope*, ret, calloc(1, sizeof(*ret)));
    *ret = (Scope) { .parent = parent, .fn = parent ? parent->fn : NULL };
    A3_HT_INIT(A3CString, ObjPtr)(&ret->idents, A3_HT_NO_HASH_KEY, A3_HT_ALLOW_GROWTH);
    A3_HT_INIT(A3CString, TypePtr)(&ret->tags, A3_HT_NO_HASH_KEY, A3_HT_ALLOW_GROWTH);
    A3_HT_INIT(A3CString, TypePtr)(&ret->typedefs, A3_HT_NO_HASH_KEY, A3_HT_ALLOW_GROWTH);

    return ret;
}

static Scope* scope_first_ancestor(Scope* scope) {
    assert(scope);

    while (scope->parent)
        scope = scope->parent;

    return scope;
}

static Obj* scope_find_in(Scope* scope, A3CString name) {
    Obj** entry = A3_HT_FIND(A3CString, ObjPtr)(&scope->idents, name);

    return entry ? *entry : NULL;
}

static Obj* scope_find(Scope* scope, A3CString name) {
    assert(scope);
    assert(name.ptr);

    Obj* ret = scope_find_in(scope, name);
    if (!ret && scope->parent)
        return scope_find(scope->parent, name);

    return ret;
}

static void scope_add(Scope* scope, Obj* obj) {
    assert(scope);
    assert(obj);
    assert(obj->name.ptr);

    bool res = A3_HT_INSERT(A3CString, ObjPtr)(&scope->idents, obj->name, obj);
    assert(res);
    (void)res;
}

static Type const* scope_find_struct_in(Scope* scope, A3CString name) {
    assert(scope);
    assert(name.ptr);

    Type const** entry = A3_HT_FIND(A3CString, TypePtr)(&scope->tags, name);
    return entry ? *entry : NULL;
}

static Type const* scope_find_struct(Scope* scope, A3CString name) {
    assert(scope);
    assert(name.ptr);

    Type const* ret = scope_find_struct_in(scope, name);
    if (!ret && scope->parent)
        return scope_find_struct(scope->parent, name);

    return ret;
}

static Type const* scope_find_typedef_in(Scope* scope, A3CString name) {
    assert(scope);
    assert(name.ptr);

    Type const** entry = A3_HT_FIND(A3CString, TypePtr)(&scope->typedefs, name);
    return entry ? *entry : NULL;
}

static Type const* scope_find_typedef(Scope* scope, A3CString name) {
    assert(scope);
    assert(name.ptr);

    Type const* ret = scope_find_typedef_in(scope, name);
    if (!ret && scope->parent)
        return scope_find_typedef(scope->parent, name);

    return ret;
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

    *ret = (Registry) {
        .src           = A3_CS_NULL,
        .current_unit  = NULL,
        .current_scope = NULL,
        .lit_count     = 0,
        .init_type     = NULL,
    };

    A3_HT_INIT(TypePtr, TypePtr)(&ret->ptrs_to, A3_HT_NO_HASH_KEY, A3_HT_ALLOW_GROWTH);
    A3_HT_INIT(A3CString, TypePtr)(&ret->fns, A3_HT_NO_HASH_KEY, A3_HT_ALLOW_GROWTH);
    reg_scope_push(ret);

    return ret;
}

A3String type_name(Type const* type) {
    assert(type);

    switch (type->type) {
    case TY_VOID:
        return a3_string_clone(A3_CS("void"));
    case TY_I8:
        return a3_string_clone(A3_CS("__i8"));
    case TY_I16:
        return a3_string_clone(A3_CS("__i16"));
    case TY_I32:
        return a3_string_clone(A3_CS("__i32"));
    case TY_I64:
        return a3_string_clone(A3_CS("__i64"));
    case TY_U8:
        return a3_string_clone(A3_CS("__u8"));
    case TY_U16:
        return a3_string_clone(A3_CS("__u16"));
    case TY_U32:
        return a3_string_clone(A3_CS("__u32"));
    case TY_U64:
        return a3_string_clone(A3_CS("__u64"));
    case TY_PTR: {
        A3String base = type_name(type->parent);
        A3String ret  = a3_string_alloc(base.len + 1);
        a3_string_copy(ret, A3_S_CONST(base));
        ret.ptr[base.len] = '*';
        a3_string_free(&base);
        return ret;
    }
    case TY_FN: {
        A3Buffer buf = { .data = type_name(type->ret) };
        buf.tail     = buf.data.len;
        a3_buf_init(&buf, buf.data.len, 512);
        a3_buf_write_str(&buf, A3_CS(" (*)("));
        if (!A3_SLL_HEAD(&type->params)) {
            a3_buf_write_str(&buf, A3_CS("void)"));
            return A3_CS_MUT(a3_buf_read_ptr(&buf));
        }

        bool first = true;
        A3_SLL_FOR_EACH (Param, param, &type->params, link) {
            if (!first)
                a3_buf_write_str(&buf, A3_CS(", "));
            first = false;

            A3String param_name = type_name(param->type);
            a3_buf_write_str(&buf, A3_S_CONST(param_name));
            a3_string_free(&param_name);
        }

        if (type->is_variadic)
            a3_buf_write_str(&buf, A3_CS(", ..."));

        a3_buf_write_byte(&buf, ')');

        return A3_CS_MUT(a3_buf_read_ptr(&buf));
    }
    case TY_ARRAY: {
        A3Buffer buf = { .data = type_name(type->parent) };
        buf.tail     = buf.data.len;
        a3_buf_init(&buf, buf.data.len, 512);
        if (type->len == TYPE_ARRAY_UNSIZED)
            a3_buf_write_str(&buf, A3_CS("[]"));
        else
            a3_buf_write_fmt(&buf, "[%zu]", type->len);
        return A3_CS_MUT(a3_buf_read_ptr(&buf));
    }
    case TY_STRUCT: {
        if (!type->name.ptr)
            return a3_string_clone(A3_CS("struct <anonymous>"));
        A3Buffer* buf = a3_buf_new(24, 512);
        a3_buf_write_fmt(buf, "struct " A3_S_F, A3_S_FORMAT(type->name));

        return A3_CS_MUT(a3_buf_read_ptr(buf));
    }
    case TY_UNION: {
        if (!type->name.ptr)
            return a3_string_clone(A3_CS("union <anonymous>"));
        A3Buffer* buf = a3_buf_new(24, 512);
        a3_buf_write_fmt(buf, "union " A3_S_F, A3_S_FORMAT(type->name));

        return A3_CS_MUT(a3_buf_read_ptr(buf));
    }
    case TY_ENUM: {
        if (!type->name.ptr)
            return a3_string_clone(A3_CS("enum <anonymous>"));
        A3Buffer* buf = a3_buf_new(24, 512);
        a3_buf_write_fmt(buf, "enum " A3_S_F, A3_S_FORMAT(type->name));

        return A3_CS_MUT(a3_buf_read_ptr(buf));
    }
    }

    A3_UNREACHABLE();
}

bool type_is_scalar(Type const* type) {
    assert(type);

    return (TY_I8 <= type->type && type->type <= TY_U64) || type->type == TY_ENUM;
}

bool type_is_scalar_value(Type const* type) {
    assert(type);

    return type_is_scalar(type) || type->type == TY_PTR;
}

static bool type_is_assignable(Type const* lhs, Type const* rhs) {
    assert(lhs);
    assert(rhs);

    return lhs == rhs || (type_is_scalar(lhs) && type_is_scalar(rhs)) ||
           (lhs->type == TY_PTR && rhs->type == TY_ARRAY && lhs->parent == rhs->parent) ||
           (lhs->type == TY_ARRAY && rhs->type == TY_ARRAY && lhs->parent == rhs->parent &&
            lhs->len == rhs->len) ||
           (lhs->type == TY_PTR && rhs->type == TY_PTR &&
            (lhs->parent->type == TY_VOID || rhs->parent->type == TY_VOID));
}

static bool type_expr_is_assignable(Type const* lhs, Expr const* rhs) {
    return type_is_assignable(lhs, rhs->res_type) ||
           (lhs->type == TY_PTR && rhs->type == EXPR_LIT && rhs->lit.type == LIT_NUM &&
            rhs->lit.num == 0);
}

Member const* type_struct_find_member(Type const* s, A3CString name) {
    assert(s);
    assert(s->type == TY_STRUCT || s->type == TY_UNION);

    A3_SLL_FOR_EACH (Member, member, &s->members, link) {
        if (a3_string_cmp(member->name, name) == 0)
            return member;
    }

    return NULL;
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

static size_t align_up(size_t n, size_t align) { return (n + align - 1) & ~(align - 1); }

static Type const* type_ptr_to(Registry* reg, Type const* type) {
    assert(reg);
    assert(type);

    Type const** entry = A3_HT_FIND(TypePtr, TypePtr)(&reg->ptrs_to, type);
    if (entry)
        return *entry;

    A3_UNWRAPNI(Type*, ret, calloc(1, sizeof(*ret)));
    *ret =
        (Type) { .type = TY_PTR, .size = sizeof(void*), .align = alignof(void*), .parent = type };

    A3_HT_INSERT(TypePtr, TypePtr)(&reg->ptrs_to, type, ret);

    return ret;
}

// TODO: Deduplicate.
static Type const* type_array_of(Type const* type, size_t len) {
    A3_UNWRAPNI(Type*, ret, calloc(1, sizeof(*ret)));
    *ret = (Type) {
        .type = TY_ARRAY, .size = len * type->size, .align = type->align, .parent = type, .len = len
    };

    return ret;
}

static Param* param_new(Type const* type) {
    assert(type);

    A3_UNWRAPNI(Param*, ret, calloc(1, sizeof(*ret)));
    ret->type = type;

    return ret;
}

static Type const* type_fn_from_ptype(Registry* reg, PType const* ptype) {
    assert(reg);
    assert(ptype);
    assert(ptype->type == PTY_FN);

    A3_UNWRAPNI(Type*, ret, calloc(1, sizeof(*ret)));
    *ret = (Type) { .type        = TY_FN,
                    .size        = sizeof(void (*)(void)),
                    .align       = alignof(void (*)(void)),
                    .ret         = type_from_ptype(reg, ptype->ret),
                    .is_variadic = ptype->attributes.is_variadic };
    A3_SLL_INIT(&ret->params);

    A3_SLL_FOR_EACH (Item, decl, &ptype->params, link) {
        assert(VERTEX(decl, item)->type == V_DECL);

        Type const* type = type_from_ptype(reg, decl->decl_ptype);
        if (type->type == TY_VOID)
            break;
        if (type->type == TY_ARRAY)
            type = type_ptr_to(reg, type->parent);

        Param* param = param_new(type);

        A3_SLL_ENQUEUE(&ret->params, param, link);
    }

    A3String name = type_name(ret);

    Type const** entry = A3_HT_FIND(A3CString, TypePtr)(&reg->fns, A3_S_CONST(name));
    if (entry) {
        free(ret);
        ret = (Type*)*entry;
    } else {
        A3String ht_name = a3_string_clone(name);
        A3_HT_INSERT(A3CString, TypePtr)(&reg->fns, A3_S_CONST(ht_name), ret);
    }

    a3_string_free(&name);
    return ret;
}

static Type const* type_aggregate_from_ptype(Registry* reg, PType* ptype) {
    assert(reg);
    assert(ptype);
    assert(ptype->type == PTY_STRUCT || ptype->type == PTY_UNION || ptype->type == PTY_ENUM);

    TypeType type = ptype->type == PTY_STRUCT  ? TY_STRUCT
                    : ptype->type == PTY_UNION ? TY_UNION
                                               : TY_ENUM;

    Type* ret        = NULL;
    bool  found_prev = false;
    if (ptype->name.text.ptr) {
        if (A3_SLL_IS_EMPTY(&ptype->members)) {
            Type const* prev = scope_find_struct(reg->current_scope, ptype->name.text);
            if (prev && type == prev->type)
                return prev;
        }

        Type const* prev = scope_find_struct_in(reg->current_scope, ptype->name.text);
        if (prev) {
            if (!A3_SLL_IS_EMPTY(&prev->members)) {
                error_at(reg->src, ptype->name,
                         "Redeclaration of existing aggregate in the same scope.");
                return NULL;
            }

            if (type != prev->type) {
                error_at(reg->src, ptype->name,
                         "Use of same tag name in same scope as different tag types.");
                return NULL;
            }

            ret        = (Type*)prev;
            found_prev = true;
        } else {
            A3_UNWRAPN(ret, calloc(1, sizeof(*ret)));
        }
    } else {
        A3_UNWRAPN(ret, calloc(1, sizeof(*ret)));
    }

    ret->type = type;
    ret->name = ptype->name.text;
    A3_SLL_INIT(&ret->members);

    size_t offset = 0;
    while (!A3_SLL_IS_EMPTY(&ptype->members)) {
        Member* member = A3_SLL_HEAD(&ptype->members);
        A3_SLL_DEQUEUE(&ptype->members, link);
        A3_SLL_ENQUEUE(&ret->members, member, link);

        if (type == TY_ENUM) {
            if (member->init) {
                EvalResult res = eval(reg->src, member->init);
                if (!res.ok)
                    return NULL;
                if (res.value < 0 || res.value > UINT32_MAX) {
                    error_at(reg->src, ptype->span, "Invalid enum constant value.");
                    return NULL;
                }

                offset = (size_t)res.value;
            }

            scope_add(reg->current_scope,
                      obj_enum_const_new(member->name, ret, (uint32_t)offset++));
            continue;
        }

        member->type   = type_from_ptype(reg, member->ptype);
        member->offset = 0;
        if (type == TY_STRUCT) {
            member->offset = offset = align_up(offset, member->type->align);
            offset += member->type->size;
        } else {
            ret->size = MAX(ret->size, member->type->size);
        }

        ret->align = MAX(ret->align, member->type->align);
    }
    if (type == TY_STRUCT) {
        ret->size = offset;
    } else if (type == TY_ENUM) {
        ret->size  = BUILTIN_TYPES[TY_I32]->size;
        ret->align = BUILTIN_TYPES[TY_I32]->align;
    }

    if (ret->name.ptr && !found_prev)
        A3_HT_INSERT(A3CString, TypePtr)(&reg->current_scope->tags, ret->name, ret);

    return ret;
}

static Type const* type_from_ptype(Registry* reg, PType* ptype) {
    assert(reg);
    assert(ptype);

    switch (ptype->type) {
    case PTY_PTR:
        return type_ptr_to(reg, type_from_ptype(reg, ptype->parent));
    case PTY_ARRAY: {
        if (!ptype->len)
            return type_array_of(type_from_ptype(reg, ptype->parent), TYPE_ARRAY_UNSIZED);

        EvalResult res = eval(reg->src, ptype->len);
        if (!res.ok)
            return NULL;
        if (res.value < 0) {
            type_error(reg, VERTEX(ptype->len, expr),
                       "Array length must be non-negative (got %" PRId64 ")", res.value);
            return NULL;
        }

        return type_array_of(type_from_ptype(reg, ptype->parent), (size_t)res.value);
    }
    case PTY_FN:
        return type_fn_from_ptype(reg, ptype);
    case PTY_STRUCT:
    case PTY_UNION:
    case PTY_ENUM:
        return type_aggregate_from_ptype(reg, ptype);
    case PTY_BUILTIN:
        switch (ptype->builtin_type) {
        case PTY_VOID:
            return BUILTIN_TYPES[TY_VOID];
        case PTY_I8:
        case PTY_I8 | PTY_SIGNED:
        case PTY_U8 | PTY_SIGNED:
        case PTY_CHAR | PTY_SIGNED:
            return BUILTIN_TYPES[TY_I8];
        case PTY_I16:
        case PTY_I16 | PTY_SIGNED:
        case PTY_U16 | PTY_SIGNED:
        case PTY_SHORT:
        case PTY_SHORT | PTY_INT:
        case PTY_SHORT | PTY_INT | PTY_SIGNED:
            return BUILTIN_TYPES[TY_I16];
        case PTY_I32:
        case PTY_I32 | PTY_SIGNED:
        case PTY_U32 | PTY_SIGNED:
        case PTY_INT:
        case PTY_INT | PTY_SIGNED:
            return BUILTIN_TYPES[TY_I32];
        case PTY_I64:
        case PTY_I64 | PTY_SIGNED:
        case PTY_U64 | PTY_SIGNED:
        case PTY_LONG:
        case PTY_LONG | PTY_INT:
        case PTY_LONG | PTY_INT | PTY_SIGNED:
        case PTY_LONG_LONG:
        case PTY_LONG_LONG | PTY_INT:
        case PTY_LONG_LONG | PTY_INT | PTY_SIGNED:
        case PTY_ISIZE:
        case PTY_ISIZE | PTY_SIGNED:
        case PTY_USIZE | PTY_SIGNED:
            return BUILTIN_TYPES[TY_I64];
        case PTY_U8:
        case PTY_U8 | PTY_UNSIGNED:
        case PTY_I8 | PTY_UNSIGNED:
        case PTY_CHAR:
        case PTY_CHAR | PTY_UNSIGNED:
            return BUILTIN_TYPES[TY_U8];
        case PTY_U16:
        case PTY_U16 | PTY_UNSIGNED:
        case PTY_I16 | PTY_UNSIGNED:
        case PTY_SHORT | PTY_UNSIGNED:
        case PTY_SHORT | PTY_INT | PTY_UNSIGNED:
            return BUILTIN_TYPES[TY_U16];
        case PTY_U32:
        case PTY_U32 | PTY_UNSIGNED:
        case PTY_I32 | PTY_UNSIGNED:
        case PTY_INT | PTY_UNSIGNED:
            return BUILTIN_TYPES[TY_U32];
        case PTY_U64:
        case PTY_U64 | PTY_UNSIGNED:
        case PTY_I64 | PTY_UNSIGNED:
        case PTY_LONG | PTY_UNSIGNED:
        case PTY_LONG | PTY_INT | PTY_UNSIGNED:
        case PTY_LONG_LONG | PTY_UNSIGNED:
        case PTY_LONG_LONG | PTY_INT | PTY_UNSIGNED:
        case PTY_USIZE:
        case PTY_USIZE | PTY_UNSIGNED:
        case PTY_ISIZE | PTY_UNSIGNED:
            return BUILTIN_TYPES[TY_U64];
        default:
            error_at(reg->src, ptype->span, "Invalid declaration type.");
            return NULL;
        }
    case PTY_DEFINED: {
        Type const* ret = scope_find_typedef(reg->current_scope, ptype->defined_name);
        if (!ret) {
            error_at(reg->src, ptype->span, "Undefined type name.");
            return NULL;
        }

        return ret;
    }
    case PTY_DUMMY:
        A3_UNREACHABLE();
    }

    A3_UNREACHABLE();
}

static bool type_bin_op(AstVisitor* visitor, BinOp* op) {
    assert(visitor);
    assert(op);

    A3_TRYB(vertex_visit(visitor, VERTEX(op->lhs, expr)));
    A3_TRYB(vertex_visit(visitor, VERTEX(op->rhs, expr)));

    switch (op->type) {
    case OP_DIV:
    case OP_MUL:
    case OP_MOD:
    case OP_SHL:
    case OP_SHR:
    case OP_BW_AND:
    case OP_BW_OR:
    case OP_BW_XOR:
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
        EXPR(op, bin_op)->res_type = op->lhs->res_type;
        break;
    case OP_ASSIGN:
        if ((!type_is_scalar(op->lhs->res_type) || !type_is_scalar(op->rhs->res_type)) &&
            op->lhs->res_type != op->rhs->res_type &&
            (op->lhs->res_type->type != TY_PTR || op->rhs->res_type->type != TY_ARRAY ||
             op->lhs->res_type->parent != op->rhs->res_type->parent)) {
            type_error_mismatch(visitor->ctx, VERTEX(op, expr.bin_op), op->lhs->res_type,
                                op->rhs->res_type);
            return false;
        }

        EXPR(op, bin_op)->res_type = op->lhs->res_type;
        break;
    case OP_EQ:
    case OP_GE:
    case OP_GT:
    case OP_LE:
    case OP_LT:
    case OP_NE:
        EXPR(op, bin_op)->res_type = BUILTIN_TYPES[TY_I32];
        break;
    case OP_OR:
    case OP_AND:
        if (!type_is_scalar_value(op->lhs->res_type) || !type_is_scalar_value(op->rhs->res_type)) {
            type_error(visitor->ctx, VERTEX(op, expr.bin_op),
                       "Non-scalar operand(s) not compatible with this operation.");
            return false;
        }

        EXPR(op, bin_op)->res_type = BUILTIN_TYPES[TY_I32];
        break;
    case OP_CAST:
        if (!type_is_scalar(op->lhs->res_type) && op->lhs->res_type->type != TY_PTR &&
            op->lhs->res_type->type != TY_VOID) {
            type_error(visitor->ctx, VERTEX(op->lhs, expr), "Invalid target type for cast.");
            return false;
        }

        EXPR(op, bin_op)->res_type = op->lhs->res_type;
        break;
    }

    assert(EXPR(op, bin_op)->res_type->type != TY_VOID || op->type == OP_CAST);
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
        if (op->operand->res_type->type != TY_PTR && op->operand->res_type->type != TY_ARRAY) {
            A3String op_name = type_name(op->operand->res_type);
            type_error(visitor->ctx, VERTEX(op->operand, expr),
                       "Tried to dereference non-pointer type " A3_S_F ".", A3_S_FORMAT(op_name));
            a3_string_free(&op_name);
            return false;
        }

        EXPR(op, unary_op)->res_type = op->operand->res_type->parent;
        break;
    case OP_NOT:
        if (!type_is_scalar(op->operand->res_type) && op->operand->res_type->type != TY_PTR) {
            A3String op_name = type_name(op->operand->res_type);
            type_error(visitor->ctx, VERTEX(op->operand, expr),
                       "Invalid operation for aggregate type " A3_S_F ".", A3_S_FORMAT(op_name));
            a3_string_free(&op_name);
            return false;
        }

        EXPR(op, unary_op)->res_type = BUILTIN_TYPES[TY_I32];
        break;
    case OP_BW_NOT:
    case OP_NEG:
    case OP_UNARY_ADD:
        if (!type_is_scalar(op->operand->res_type)) {
            A3String op_name = type_name(op->operand->res_type);
            type_error(visitor->ctx, VERTEX(op->operand, expr),
                       "Invalid operation for non-scalar type " A3_S_F ".", A3_S_FORMAT(op_name));
            a3_string_free(&op_name);
            return false;
        }

        EXPR(op, unary_op)->res_type = op->operand->res_type;
        break;
    case OP_SIZEOF:
        if (op->operand->res_type->type == TY_ARRAY &&
            op->operand->res_type->size == TYPE_ARRAY_UNSIZED) {
            type_error(visitor->ctx, VERTEX(op, expr.unary_op),
                       "Operand of sizeof cannot be an incomplete type.");
            return false;
        }

        EXPR(op, unary_op)->res_type = BUILTIN_TYPES[TY_U64];
        break;
    }

    assert(EXPR(op, unary_op)->res_type->type != TY_VOID);
    return true;
}

static A3CString type_lit_name(Registry* reg) {
    assert(reg);

    A3Buffer* buf = a3_buf_new(32, 128);

    if (!a3_buf_write_fmt(buf, "__4cc_lit%zu", reg->lit_count++))
        return A3_CS_NULL;

    return a3_buf_read_ptr(buf);
}

static bool type_lit(AstVisitor* visitor, Literal* lit) {
    assert(visitor);
    assert(lit);

    Registry* reg = visitor->ctx;

    switch (lit->type) {
    case LIT_NUM:
        EXPR(lit, lit)->res_type = BUILTIN_TYPES[TY_I32];
        break;
    case LIT_STR: {
        // TODO: There really should be a separate resolution pass for this and other things like
        // it...

        EXPR(lit, lit)->res_type = type_array_of(BUILTIN_TYPES[TY_U8], lit->str.len + 1);

        // Synthesize global declaration for storage.
        A3CString global_name = type_lit_name(reg);
        Span      span        = SPAN(lit, expr.lit);
        Item*     global_decl =
            vertex_decl_new(span, global_name,
                            ptype_array_new(span, ptype_builtin_new(span, PTY_CHAR),
                                            vertex_lit_num_new(span, BUILTIN_TYPES[TY_USIZE],
                                                               (int64_t)(lit->str.len + 1))));
        A3_SLL_PUSH(&reg->current_unit->items, global_decl, link);

        Scope* current = reg->current_scope;

        reg->current_scope = scope_first_ancestor(current);
        A3_TRYB(vertex_visit(visitor, VERTEX(global_decl, item)));
        reg->current_scope = current;

        lit->storage = global_decl->obj = scope_find(reg->current_scope, global_name);
        assert(lit->storage);
        global_decl->obj->init = vertex_init_expr_new(SPAN(lit, expr.lit), EXPR(lit, lit));
        vertex_init_lit_str_to_list(global_decl->obj->init);
        break;
    }
    }

    return true;
}

static bool type_fn(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(decl);
    assert(VERTEX(decl, item)->type == V_DECL && decl->decl_ptype->type == PTY_FN);

    Registry* reg  = visitor->ctx;
    Obj*      prev = scope_find(reg->current_scope, decl->name);

    if (prev && prev->is_defined && decl->body) {
        type_error(reg, VERTEX(decl, item), "Redefinition of already-defined function.");
        return false;
    }

    Type const* fn_type = type_from_ptype(reg, decl->decl_ptype);
    if (prev && fn_type != prev->type) {
        type_error_mismatch(reg, VERTEX(decl, item), prev->type, decl->decl_type);
        return false;
    }

    if (prev && prev->is_defined) {
        decl->decl_type = fn_type;
        return true;
    }

    Scope* fn_scope    = NULL;
    size_t stack_depth = 0;
    if (decl->body) {
        reg_scope_push(reg);
        fn_scope = reg->current_scope;

        if (!A3_SLL_IS_EMPTY(&decl->decl_ptype->params) &&
            (A3_SLL_HEAD(&decl->decl_ptype->params)->decl_ptype->builtin_type & PTY_VOID)) {
            A3_SLL_POP(&decl->decl_ptype->params, link);

            if (!A3_SLL_IS_EMPTY(&decl->decl_ptype->params)) {
                type_error(
                    reg, VERTEX(A3_SLL_HEAD(&decl->decl_ptype->params), item),
                    "Function with void parameter must have otherwise empty parameter list.");
                return false;
            }
        }

        A3_SLL_FOR_EACH (Item, param, &decl->decl_ptype->params, link) {
            param->decl_type = type_from_ptype(reg, param->decl_ptype);
            if (param->decl_type->type == TY_ARRAY)
                param->decl_type = type_ptr_to(reg, param->decl_type->parent);

            stack_depth = align_up(stack_depth, param->decl_type->align);
            stack_depth += param->decl_type->size;

            param->obj = obj_new(param->name, param->decl_type, NULL, stack_depth, OBJ_LOCAL);
            scope_add(reg->current_scope, param->obj);
        }

        reg_scope_pop(reg);
    }

    if (prev) {
        decl->obj             = prev;
        decl->obj->is_defined = decl->obj->is_defined || decl->body;
    } else {
        decl->obj = obj_fn_new(decl->name, fn_type, decl->body ? OBJ_FN_DEFINED : OBJ_FN_UNDEFINED);
        scope_add(reg->current_scope, decl->obj);
    }

    if (decl->body) {
        A3_SLL_INIT(&decl->obj->params);

        A3_SLL_FOR_EACH (Item, param, &decl->decl_ptype->params, link) {
            A3_SLL_ENQUEUE(&decl->obj->params, param, link);
        }

        fn_scope->fn           = decl->obj;
        decl->obj->scope       = fn_scope;
        decl->obj->stack_depth = stack_depth;

        reg->current_scope = fn_scope;
        decl->body->scope  = fn_scope;
        A3_TRYB(vertex_visit(visitor, VERTEX(decl->body, item.block)));
        reg_scope_pop(reg);

        decl->obj->stack_depth = align_up(decl->obj->stack_depth, 16);
    }

    decl->decl_type = fn_type;

    return true;
}

static bool type_typedef(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(decl);
    assert(VERTEX(decl, item)->type == V_DECL);
    assert(decl->attributes.is_typedef);

    Registry* reg = visitor->ctx;

    if (decl->init) {
        type_error(reg, VERTEX(decl, item), "typedef declaration cannot have an initializer.");
        return false;
    }

    Type const* prev = scope_find_typedef_in(reg->current_scope, decl->name);
    Type const* type = type_from_ptype(reg, decl->decl_ptype);
    if (prev) {
        if (type != prev) {
            type_error_mismatch(reg, VERTEX(decl, item), prev, type);
            return false;
        }

        return true;
    }

    A3_HT_INSERT(A3CString, TypePtr)(&reg->current_scope->typedefs, decl->name, type);
    return true;
}

static bool type_init(AstVisitor* visitor, Init* init) {
    assert(visitor);
    assert(init);

    Registry*   reg       = visitor->ctx;
    bool        global    = !reg->current_scope->fn;
    Type const* decl_type = reg->init_type;
    assert(decl_type);

    // Initializers of the form
    //   char x[] = "abc";
    // should be treated the same as
    //   char x[] = { 'a', 'b', 'c', '\0' };
    // rather than producing a global string literal.
    //
    // TODO: This check can probably be moved into the parser.
    if (init->type == INIT_EXPR && init->expr->type == EXPR_LIT &&
        init->expr->lit.type == LIT_STR && decl_type->type == TY_ARRAY)
        vertex_init_lit_str_to_list(init);

    switch (init->type) {
    case INIT_EXPR:
        A3_TRYB(vertex_visit(visitor, VERTEX(init->expr, expr)));

        if (!type_expr_is_assignable(decl_type, init->expr)) {
            type_error_mismatch(reg, VERTEX(init, init), decl_type, init->expr->res_type);
            return false;
        }

        if (global) {
            if (init->expr->type != EXPR_LIT) {
                type_error(reg, VERTEX(init, init),
                           "Initialization of global variable with non-literal value.");
                return false;
            }

            if (decl_type->type == TY_ARRAY && decl_type->parent->type != TY_U8) {
                type_error(reg, VERTEX(init, init), "Unsupported global array type (TODO).");
                return false;
            }

            if (decl_type->type == TY_ARRAY && init->expr->lit.type != LIT_STR) {
                type_error(reg, VERTEX(init, init),
                           "Initialization of global array with incompatible literal.");
                return false;
            }
        }

        return true;
    case INIT_LIST: {
        if (decl_type->type != TY_ARRAY) {
            A3String name = type_name(decl_type);
            type_error(reg, VERTEX(init, init),
                       "Initializer list is not assignable to type " A3_S_F ".", A3_S_FORMAT(name));
            a3_string_free(&name);
            return false;
        }

        size_t count_max = decl_type->len;
        size_t count     = 0;
        reg->init_type   = decl_type->parent;
        A3_SLL_FOR_EACH (Init, elem, &init->list, link) {
            A3_TRYB(vertex_visit(visitor, VERTEX(elem, init)));

            count++;
            if (count > count_max) {
                type_error(reg, VERTEX(init, init), "Initializer list is too long (%zu > %zu).",
                           count, count_max);
                return false;
            }
        }
        reg->init_type = decl_type;

        if (decl_type->len == TYPE_ARRAY_UNSIZED) {
            ((Type*)decl_type)->len  = count;
            ((Type*)decl_type)->size = decl_type->len * decl_type->parent->size;
        }

        return true;
    }
    }

    A3_UNREACHABLE();
}

static bool type_decl(AstVisitor* visitor, Item* decl) {
    assert(visitor);
    assert(decl);
    assert(VERTEX(decl, item)->type == V_DECL);

    Registry* reg    = visitor->ctx;
    bool      global = !reg->current_scope->fn;

    if (decl->attributes.is_typedef)
        return type_typedef(visitor, decl);

    if (decl->decl_ptype->type == PTY_FN)
        return type_fn(visitor, decl);

    if (decl->decl_ptype->type == PTY_ARRAY && decl->decl_ptype->len)
        A3_TRYB(vertex_visit(visitor, VERTEX(decl->decl_ptype->len, expr)));

    Type const* type = type_from_ptype(reg, decl->decl_ptype);
    if (!type)
        return false;
    decl->decl_type = type;

    if (type->type == TY_VOID) {
        type_error(reg, VERTEX(decl, item), "Cannot declare object of type void.");
        return false;
    }

    Obj* prev = scope_find_in(reg->current_scope, decl->name);
    if (prev) {
        if (prev->type != type) {
            type_error_mismatch(reg, VERTEX(decl, item), prev->type, type);
            return false;
        }

        if (!global) {
            type_error(reg, VERTEX(decl, item), "Redeclaration of existing item.");
            return false;
        }
    }

    if (!decl->name.ptr &&
        (type->type == TY_STRUCT || type->type == TY_UNION || type->type == TY_ENUM) &&
        (type->name.ptr || type->type == TY_ENUM))
        return true;

    if (decl->init) {
        if (decl->attributes.is_extern) {
            type_error(reg, VERTEX(decl, item),
                       "extern-qualified declaration cannot have an initializer.");
            return false;
        }

        reg->init_type = type;
        A3_TRYB(vertex_visit(visitor, VERTEX(decl->init, init)));
        reg->init_type = NULL;
    } else if (type->type == TY_ARRAY && type->len == TYPE_ARRAY_UNSIZED &&
               !decl->attributes.is_extern) {
        type_error(reg, VERTEX(decl, item), "Declaration of array with incomplete type.");
        return false;
    }

    if (prev && decl->init) {
        if (prev->init) {
            type_error(reg, VERTEX(decl->init, expr), "Reinitialization of global variable.");
            return false;
        }

        prev->init = decl->init;
        decl->init = NULL;
    }

    if (!global) {
        reg->current_scope->fn->stack_depth =
            align_up(reg->current_scope->fn->stack_depth, type->align) + type->size;
    }

    if (prev) {
        decl->obj = prev;
    } else {
        decl->obj = obj_new(decl->name, type, decl->init,
                            !global ? reg->current_scope->fn->stack_depth : 0, global);
        scope_add(reg->current_scope, decl->obj);
    }

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

    var->obj = scope_find(reg->current_scope, var->name);
    if (!var->obj) {
        type_error(reg, VERTEX(var, expr.var), "Variable used without declaration.");
        return false;
    }

    if (var->obj->is_named_literal) {
        Expr* lit       = vertex_lit_num_new(SPAN(var, expr.var), var->obj->type, var->obj->value);
        *EXPR(var, var) = *lit;
        free(VERTEX(lit, expr));
    } else {
        EXPR(var, var)->res_type = var->obj->type;
    }

    return true;
}

static bool type_call(AstVisitor* visitor, Call* call) {
    assert(visitor);
    assert(call);

    Registry* reg = visitor->ctx;

    call->obj = scope_find(reg->current_scope, call->name);
    if (!call->obj) {
        type_error(reg, VERTEX(call, expr.call), "Call of undeclared function.");
        return false;
    }

    if (call->obj->type->type != TY_FN) {
        type_error(reg, VERTEX(call, expr.call), "Call of non-function object.");
        return false;
    }

    EXPR(call, call)->res_type = call->obj->type->ret;

    Param* param = A3_SLL_HEAD(&call->obj->type->params);
    A3_SLL_FOR_EACH (Arg, arg, &call->args, link) {
        A3_TRYB(vertex_visit(visitor, VERTEX(arg->expr, expr)));

        if (!param) {
            if (call->obj->type->is_variadic)
                continue;

            type_error(reg, VERTEX(arg->expr, expr),
                       "More arguments in call than function has parameters.");
            return false;
        }

        if (!type_expr_is_assignable(param->type, arg->expr)) {
            type_error_mismatch(reg, VERTEX(arg->expr, expr), param->type, arg->expr->res_type);
            return false;
        }

        param = A3_SLL_NEXT(param, link);
    }
    if (param) {
        type_error(reg, VERTEX(call, expr.call), "Call with fewer arguments than required.");
        return false;
    }

    return true;
}

static bool type_member(AstVisitor* visitor, MemberAccess* member) {
    assert(visitor);
    assert(member);

    A3_TRYB(vertex_visit(visitor, VERTEX(member->lhs, expr)));
    Member const* mem = type_struct_find_member(member->lhs->res_type, member->name);
    if (!mem) {
        type_error(visitor->ctx, VERTEX(member, expr.member), "No member of this name exists.");
        return false;
    }

    member->rhs                    = mem;
    EXPR(member, member)->res_type = mem->type;

    return true;
}

static bool type_expr_cond(AstVisitor* visitor, CondExpr* expr) {
    assert(visitor);
    assert(expr);

    A3_TRYB(vertex_visit(visitor, VERTEX(expr->cond, expr)));
    if (!type_is_scalar(expr->cond->res_type)) {
        type_error(visitor->ctx, VERTEX(expr->cond, expr),
                   "Non-scalar type cannot be a condition.");
        return false;
    }

    if (expr->res_true)
        A3_TRYB(vertex_visit(visitor, VERTEX(expr->res_true, expr)));
    A3_TRYB(vertex_visit(visitor, VERTEX(expr->res_false, expr)));

    Type const* res_true  = (expr->res_true ?: expr->cond)->res_type;
    Type const* res_false = expr->res_false->res_type;
    if (res_true != res_false) {
        type_error_mismatch(visitor->ctx, VERTEX(expr, expr.cond), res_true, res_false);
        return false;
    }

    EXPR(expr, cond)->res_type = res_true;
    return true;
}

static bool type_expr_type(AstVisitor* visitor, Expr* expr) {
    assert(visitor);
    assert(expr);
    assert(expr->type == EXPR_TYPE);

    return (expr->res_type = type_from_ptype(visitor->ctx, expr->res_ptype));
}

static bool type_block(AstVisitor* visitor, Block* block) {
    assert(visitor);
    assert(block);

    Registry* reg       = visitor->ctx;
    bool      has_scope = block->scope;

    if (!has_scope) {
        reg_scope_push(reg);
        block->scope = reg->current_scope;
    }

    A3_SLL_FOR_EACH (Item, item, &block->body, link) {
        A3_TRYB(vertex_visit(visitor, VERTEX(item, item)));
    }

    if (!has_scope)
        reg_scope_pop(reg);

    return true;
}

static bool type_loop(AstVisitor* visitor, Loop* loop) {
    assert(visitor);
    assert(loop);

    Registry* reg = visitor->ctx;
    reg_scope_push(reg);

    if (loop->init) {
        if (loop->init->type == STMT_BLOCK)
            loop->init->block.scope = reg->current_scope;

        A3_TRYB(vertex_visit(visitor, VERTEX(loop->init, item)));
    }
    if (loop->cond)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->cond, expr)));

    reg_scope_push(reg);
    if (loop->body->type == STMT_BLOCK)
        loop->body->block.scope = reg->current_scope;
    A3_TRYB(vertex_visit(visitor, VERTEX(loop->body, item)));
    reg_scope_pop(reg);

    if (loop->post)
        A3_TRYB(vertex_visit(visitor, VERTEX(loop->post, expr)));

    reg_scope_pop(reg);
    return true;
}

bool type(Registry* reg, A3CString src, Vertex* root) {
    assert(reg);
    assert(src.ptr);
    assert(root->type == V_UNIT);

    reg->src          = src;
    reg->current_unit = &root->unit;

    return vertex_visit(
        &(AstVisitor) {
            .ctx             = reg,
            .visit_bin_op    = type_bin_op,
            .visit_unary_op  = type_unary_op,
            .visit_lit       = type_lit,
            .visit_var       = type_var,
            .visit_call      = type_call,
            .visit_member    = type_member,
            .visit_expr_cond = type_expr_cond,
            .visit_expr_type = type_expr_type,
            .visit_block     = type_block,
            .visit_loop      = type_loop,
            .visit_decl      = type_decl,
            .visit_init      = type_init,
        },
        root);
}

TypeType type_to_underlying(TypeType type) {
    switch (type) {
    case TY_ENUM:
        return TY_U32;
    case TY_PTR:
        return TY_USIZE;
    default:
        return type;
    }
}

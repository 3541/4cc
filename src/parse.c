/*
 * PARSE -- Parser.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include "parse.h"

#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>

#include <a3/buffer.h>
#include <a3/str.h>
#include <a3/util.h>
#include <a3/vec.h>

#include "ast.h"
#include "error.h"
#include "lex.h"

typedef struct PTypeScope PTypeScope;
typedef struct PTypeScope {
    A3Vec       typedefs;
    PTypeScope* parent;
} PTypeScope;

typedef struct Parser {
    A3CString   src;
    Lexer*      lexer;
    Unit*       current_unit;
    PTypeScope* current_scope;
    size_t      error_depth;
    bool        status;
} Parser;

static Expr*  parse_expr(Parser*, uint8_t precedence);
static Item*  parse_stmt(Parser*);
static Block* parse_block(Parser*);
static Item*  parse_declarator(Parser*, PType*);
static PType* parse_decl_suffix(Parser*, PType*);
static PType* parse_declspec(Parser*);
static bool   parse_decl(Parser*, Items*);

static void parse_scope_push(Parser* parser) {
    assert(parser);

    A3_UNWRAPNI(PTypeScope*, scope, calloc(1, sizeof(*scope)));
    A3_VEC_INIT(A3CString, &scope->typedefs);
    scope->parent         = parser->current_scope;
    parser->current_scope = scope;
}

static void parse_scope_pop(Parser* parser) {
    assert(parser);
    assert(parser->current_scope);

    parser->current_scope = parser->current_scope->parent;
}

static void parse_scope_typedef_add(Parser* parser, A3CString name) {
    assert(parser);
    assert(parser->current_scope);

    A3_VEC_PUSH(&parser->current_scope->typedefs, &name);
}

static bool parse_scope_typedef_find_in(PTypeScope* scope, A3CString name) {
    assert(scope);

    A3_VEC_FOR_EACH(A3CString, type, &scope->typedefs) {
        if (a3_string_cmp(*type, name) == 0)
            return true;
    }

    return false;
}

static bool parse_scope_typedef_find(PTypeScope* scope, A3CString name) {
    assert(scope);

    if (parse_scope_typedef_find_in(scope, name))
        return true;

    if (!scope->parent)
        return false;

    return parse_scope_typedef_find(scope->parent, name);
}

Parser* parse_new(A3CString src, Lexer* lexer) {
    A3_UNWRAPNI(Parser*, ret, calloc(1, sizeof(*ret)));
    *ret = (Parser) { .src           = src,
                      .lexer         = lexer,
                      .current_unit  = NULL,
                      .current_scope = NULL,
                      .error_depth   = 0,
                      .status        = true };
    parse_scope_push(ret);

    return ret;
}

A3_FORMAT_FN(3, 4)
static void parse_error(Parser* parser, Token tok, char* fmt, ...) {
    assert(parser);

    parser->status = false;

    va_list args;
    va_start(args, fmt);

    verror_at(parser->src, tok.lexeme, fmt, args);

    va_end(args);
}

static bool parse_consume(Parser* parser, A3CString name, TokenType token) {
    assert(parser);

    Token t = lex_next(parser->lexer);
    if (t.type != token) {
        parse_error(parser, t, "Expected a " A3_S_F ".", A3_S_FORMAT(name));
        return false;
    }

    return true;
}

static Span parse_span_merge(Span lhs, Span rhs) {
    assert(lhs.text.ptr);
    assert(rhs.text.ptr);
    assert(lhs.text.ptr <= rhs.text.ptr);

    return (Span) { .line = lhs.line,
                    .text = a3_cstring_new(lhs.text.ptr,
                                           (size_t)(rhs.text.ptr + rhs.text.len - lhs.text.ptr)) };
}

static bool parse_has_next(Parser* parser) {
    assert(parser);

    Token next = lex_peek(parser->lexer);
    return next.type != TOK_EOF && next.type != TOK_ERR;
}

static bool parse_has_decl_builtin(Parser* parser) {
    assert(parser);

    Token next = lex_peek(parser->lexer);
    return next.type == TOK_VOID || next.type == TOK_I8 || next.type == TOK_I16 ||
           next.type == TOK_I32 || next.type == TOK_I64 || next.type == TOK_ISIZE ||
           next.type == TOK_U8 || next.type == TOK_U16 || next.type == TOK_U32 ||
           next.type == TOK_U64 || next.type == TOK_USIZE || next.type == TOK_CHAR ||
           next.type == TOK_SHORT || next.type == TOK_INT || next.type == TOK_LONG ||
           next.type == TOK_SIGNED || next.type == TOK_UNSIGNED;
}

static bool parse_has_decl_aggregate(Parser* parser) {
    assert(parser);

    Token next = lex_peek(parser->lexer);
    return next.type == TOK_STRUCT || next.type == TOK_UNION;
}

static bool parse_has_defined_type(Parser* parser) {
    assert(parser);

    Token next = lex_peek(parser->lexer);
    return next.type == TOK_IDENT &&
           parse_scope_typedef_find(parser->current_scope, next.lexeme.text);
}

static bool parse_has_decl(Parser* parser) {
    assert(parser);

    Token next = lex_peek(parser->lexer);
    return parse_has_decl_builtin(parser) || parse_has_decl_aggregate(parser) ||
           parse_has_defined_type(parser) || next.type == TOK_TYPEDEF;
}

static BinOpType parse_bin_op(TokenType type) {
    switch (type) {
    case TOK_PLUS:
        return OP_ADD;
    case TOK_MINUS:
        return OP_SUB;
    case TOK_STAR:
        return OP_MUL;
    case TOK_SLASH:
        return OP_DIV;
    case TOK_EQ_EQ:
        return OP_EQ;
    case TOK_BANG_EQ:
        return OP_NE;
    case TOK_LT:
        return OP_LT;
    case TOK_LT_EQ:
        return OP_LE;
    case TOK_GT:
        return OP_GT;
    case TOK_GT_EQ:
        return OP_GE;
    case TOK_EQ:
        return OP_ASSIGN;
    case TOK_AMP_AMP:
        return OP_AND;
    case TOK_PIPE_PIPE:
        return OP_OR;
    case TOK_PERCENT:
        return OP_MOD;
    case TOK_AMP:
        A3_PANIC("TODO");
    default:
        A3_PANIC("Not a binary operator.");
    }
}

static UnaryOpType parse_unary_op(TokenType type) {
    switch (type) {
    case TOK_PLUS:
        return OP_UNARY_ADD;
    case TOK_MINUS:
        return OP_NEG;
    case TOK_AMP:
        return OP_ADDR;
    case TOK_STAR:
        return OP_DEREF;
    case TOK_BANG:
        return OP_NOT;
    case TOK_TILDE:
        return OP_BW_NOT;
    case TOK_SIZEOF:
        return OP_SIZEOF;
    default:
        A3_PANIC("Not a unary operator.");
    }
}

static Expr* parse_var(Parser* parser) {
    assert(parser);

    Token tok = lex_next(parser->lexer);
    assert(tok.type == TOK_IDENT);

    return vertex_var_new(tok.lexeme, tok.lexeme.text);
}

static Expr* parse_call(Parser* parser, Expr* callee) {
    assert(parser);
    assert(callee);

    if (callee->type != EXPR_VAR) {
        parse_error(parser, lex_next(parser->lexer), "Call of non-function.");
        return NULL;
    }

    Token tok_left = lex_next(parser->lexer);
    assert(tok_left.type == TOK_LPAREN);

    Expr* ret =
        vertex_call_new(parse_span_merge(SPAN(callee, expr), tok_left.lexeme), callee->var.name);

    bool first = true;
    while (parse_has_next(parser) && lex_peek(parser->lexer).type != TOK_RPAREN) {
        if (!first && !parse_consume(parser, A3_CS("comma"), TOK_COMMA))
            return NULL;
        first = false;

        Expr* expr = parse_expr(parser, 0);
        if (!expr)
            return NULL;

        Arg* arg = arg_new(expr);
        A3_SLL_ENQUEUE(&ret->call.args, arg, link);
    }

    Token tok_right = lex_next(parser->lexer);
    if (tok_right.type != TOK_RPAREN) {
        parse_error(parser, tok_right, "Expected a closing parenthesis.");
        return NULL;
    }

    SPAN(ret, expr) = parse_span_merge(SPAN(ret, expr), tok_right.lexeme);
    return ret;
}

static Expr* parse_index(Parser* parser, Expr* base) {
    assert(parser);
    assert(base);

    Token tok_left = lex_next(parser->lexer);
    assert(tok_left.type == TOK_LBRACKET);

    Expr* index = parse_expr(parser, 0);

    Token next = lex_next(parser->lexer);
    if (next.type != TOK_RBRACKET) {
        parse_error(parser, next, "Expected a closing bracket.");
        return NULL;
    }

    return vertex_unary_op_new(
        parse_span_merge(SPAN(base, expr), next.lexeme), OP_DEREF,
        vertex_bin_op_new(parse_span_merge(tok_left.lexeme, next.lexeme), OP_ADD, base, index));
}

static Expr* parse_lit_str(Parser* parser) {
    assert(parser);

    Token tok = lex_next(parser->lexer);
    assert(tok.type == TOK_LIT_STR);

    return vertex_lit_str_new(tok.lexeme, tok.lit_str);
}

static uint8_t PREFIX_PRECEDENCE[TOK_COUNT] = {
    [TOK_PLUS] = 17, [TOK_MINUS] = 17, [TOK_AMP] = 17,    [TOK_STAR] = 17,
    [TOK_BANG] = 17, [TOK_TILDE] = 17, [TOK_SIZEOF] = 17,
};

static uint8_t INFIX_PRECEDENCE[TOK_COUNT][2] = {
    [TOK_EQ] = { 2, 1 },

    [TOK_QUERY] = { 4, 3 },

    [TOK_PIPE_PIPE] = { 5, 6 },

    [TOK_AMP_AMP] = { 7, 8 },

    [TOK_EQ_EQ] = { 9, 10 },    [TOK_BANG_EQ] = { 9, 10 },

    [TOK_GT] = { 11, 12 },      [TOK_GT_EQ] = { 11, 12 },
    [TOK_LT] = { 11, 12 },      [TOK_LT_EQ] = { 11, 12 },

    [TOK_PLUS] = { 13, 14 },    [TOK_MINUS] = { 13, 14 },

    [TOK_STAR] = { 15, 16 },    [TOK_SLASH] = { 15, 16 }, [TOK_PERCENT] = { 15, 16, },

    [TOK_DOT] = { 19, 20 },     [TOK_MINUS_GT] = { 19, 20 }
};

static uint8_t POSTFIX_PRECEDENCE[TOK_COUNT] = {
    [TOK_LPAREN]   = 19,
    [TOK_LBRACKET] = 19,
};

static Expr* parse_expr(Parser* parser, uint8_t precedence) {
    assert(parser);

    Expr* lhs = NULL;
    Token tok = lex_peek(parser->lexer);
    switch (tok.type) {
    case TOK_LPAREN:
        lex_next(parser->lexer);
        lhs = parse_expr(parser, 0);
        if (!parse_consume(parser, A3_CS("closing parenthesis"), TOK_RPAREN))
            return NULL;
        break;
    case TOK_LIT_NUM: {
        lex_next(parser->lexer);
        lhs = vertex_lit_num_new(tok.lexeme, tok.lit_num);
        break;
    }
    case TOK_LIT_STR:
        lhs = parse_lit_str(parser);
        break;
    case TOK_IDENT:
        lhs = parse_var(parser);
        break;
    default:
        if (!PREFIX_PRECEDENCE[tok.type]) {
            parse_error(parser, lex_next(parser->lexer),
                        "Expected a literal, opening parenthesis, or unary operator.");

            return NULL;
        }

        lex_next(parser->lexer);
        Expr* rhs = parse_expr(parser, PREFIX_PRECEDENCE[tok.type]);
        if (!rhs)
            return NULL;

        lhs = vertex_unary_op_new(parse_span_merge(tok.lexeme, SPAN(rhs, expr)),
                                  parse_unary_op(tok.type), rhs);
        break;

        return NULL;
    }

    while (true) {
        Token tok_op = lex_peek(parser->lexer);

        if (POSTFIX_PRECEDENCE[tok_op.type]) {
            if (POSTFIX_PRECEDENCE[tok_op.type] < precedence)
                break;

            switch (tok_op.type) {
            case TOK_LPAREN:
                lhs = parse_call(parser, lhs);
                break;
            case TOK_LBRACKET:
                lhs = parse_index(parser, lhs);
                break;
            default:
                A3_PANIC("Todo: other postfix operators.");
            }

            continue;
        }

        if (!lhs)
            return NULL;

        if (!INFIX_PRECEDENCE[tok_op.type][0] || INFIX_PRECEDENCE[tok_op.type][0] < precedence)
            break;

        lex_next(parser->lexer);

        if (tok_op.type == TOK_QUERY) {
            Expr* res_true = NULL;
            if (lex_peek(parser->lexer).type != TOK_COLON) {
                res_true = parse_expr(parser, 0);
                if (!res_true)
                    return NULL;
            }
            if (!parse_consume(parser, A3_CS("colon"), TOK_COLON))
                return NULL;

            Expr* res_false = parse_expr(parser, INFIX_PRECEDENCE[TOK_QUERY][1]);
            if (!res_false)
                return NULL;

            lhs = vertex_expr_cond_new(parse_span_merge(SPAN(lhs, expr), SPAN(res_false, expr)),
                                       lhs, res_true, res_false);
            continue;
        }

        Expr* rhs = parse_expr(parser, INFIX_PRECEDENCE[tok_op.type][1]);
        if (!rhs)
            return NULL;

        Span op_span = parse_span_merge(SPAN(lhs, expr), SPAN(rhs, expr));
        if (tok_op.type == TOK_DOT || tok_op.type == TOK_MINUS_GT) {
            if (rhs->type != EXPR_VAR) {
                parse_error(parser, tok_op,
                            "Right-hand side of member access must be an identifier.");
                return NULL;
            }

            if (tok_op.type == TOK_MINUS_GT) {
                lhs = vertex_unary_op_new(parse_span_merge(SPAN(lhs, expr), tok_op.lexeme),
                                          OP_DEREF, lhs);
            }

            lhs = vertex_member_new(op_span, lhs, rhs->var.name);
        } else {
            lhs = vertex_bin_op_new(op_span, parse_bin_op(tok_op.type), lhs, rhs);
        }
    }

    return lhs;
}

static Item* parse_expr_stmt(Parser* parser) {
    assert(parser);

    Expr* expr = parse_expr(parser, 0);
    if (!expr)
        return NULL;

    Token next = lex_next(parser->lexer);
    if (next.type != TOK_SEMI) {
        parse_error(parser, next, "Expected a semicolon.");
        return NULL;
    }

    return vertex_expr_stmt_new(parse_span_merge(SPAN(expr, expr), next.lexeme), expr);
}

static Item* parse_ret(Parser* parser) {
    assert(parser);

    Token tok = lex_next(parser->lexer);
    assert(tok.type == TOK_RET);

    Expr* expr = parse_expr(parser, 0);
    if (!expr)
        return NULL;

    Token next = lex_next(parser->lexer);
    if (next.type == TOK_EOF)
        return NULL;
    if (next.type != TOK_SEMI) {
        parse_error(parser, next, "Expected a semicolon.");
        return NULL;
    }

    return vertex_ret_new(parse_span_merge(tok.lexeme, SPAN(expr, expr)), expr);
}

static Item* parse_if(Parser* parser) {
    assert(parser);

    Token if_tok = lex_next(parser->lexer);
    assert(if_tok.type == TOK_IF);

    if (!parse_consume(parser, A3_CS("opening parenthesis"), TOK_LPAREN))
        return NULL;

    Expr* cond = parse_expr(parser, 0);
    if (!cond)
        return NULL;

    if (!parse_consume(parser, A3_CS("closing parenthesis"), TOK_RPAREN))
        return NULL;

    Item* body_true  = parse_stmt(parser);
    Item* body_false = NULL;

    if (!body_true)
        return NULL;

    if (lex_peek(parser->lexer).type == TOK_ELSE) {
        lex_next(parser->lexer);
        body_false = parse_stmt(parser);
        if (!body_false)
            return NULL;
    }

    return ITEM(vertex_if_new(parse_span_merge(if_tok.lexeme, body_false ? SPAN(body_false, item)
                                                                         : SPAN(body_true, item)),
                              cond, body_true, body_false),
                if_stmt);
}

static Item* parse_loop(Parser* parser) {
    assert(parser);

    Token loop_tok = lex_next(parser->lexer);
    assert(loop_tok.type == TOK_FOR || loop_tok.type == TOK_WHILE || loop_tok.type == TOK_DO);

    if (loop_tok.type != TOK_DO && !parse_consume(parser, A3_CS("opening parenthesis"), TOK_LPAREN))
        return NULL;

    Item* init = NULL;
    Expr* cond = NULL;
    Expr* post = NULL;

    if (loop_tok.type == TOK_FOR) {
        if (parse_has_decl(parser)) {
            Block* init_block = vertex_block_new();
            if (!parse_decl(parser, &init_block->body))
                return NULL;
            init = ITEM(init_block, block);
        } else {
            if (lex_peek(parser->lexer).type == TOK_SEMI) {
                Token next = lex_next(parser->lexer);
                init       = vertex_empty_new(next.lexeme);
            } else {
                init = parse_expr_stmt(parser);
            }

            if (init->type != STMT_EMPTY &&
                (init->type != STMT_EXPR_STMT || init->expr->type != EXPR_BIN_OP ||
                 init->expr->bin_op.type != OP_ASSIGN)) {
                error_at(parser->src, SPAN(init, item), "Expected an assignment expression.");
                return NULL;
            }
        }

        if (!init)
            return NULL;

        if (lex_peek(parser->lexer).type != TOK_SEMI)
            cond = parse_expr(parser, 0);
        if (!parse_consume(parser, A3_CS("semicolon"), TOK_SEMI))
            return NULL;

        if (lex_peek(parser->lexer).type != TOK_RPAREN) {
            post = parse_expr(parser, 0);
            if (!post)
                return NULL;
        }
    }
    if (loop_tok.type == TOK_WHILE) {
        cond = parse_expr(parser, 0);
        if (!cond)
            return NULL;
    }

    if (loop_tok.type != TOK_DO && !parse_consume(parser, A3_CS("closing parenthesis"), TOK_RPAREN))
        return NULL;

    Item* body = parse_stmt(parser);
    if (!body)
        return NULL;

    if (loop_tok.type == TOK_DO) {
        assert(!cond);

        if (!parse_consume(parser, A3_CS("while"), TOK_WHILE))
            return NULL;

        cond = parse_expr(parser, 0);
        if (!cond)
            return NULL;

        if (!parse_consume(parser, A3_CS("semicolon"), TOK_SEMI))
            return NULL;
    }

    return ITEM(vertex_loop_new(parse_span_merge(loop_tok.lexeme, SPAN(body, item)),
                                loop_tok.type == TOK_DO ? LOOP_COND_POST : LOOP_COND_PRE, init,
                                cond, post, body),
                loop);
}

static Item* parse_stmt(Parser* parser) {
    assert(parser);

    Token next = lex_peek(parser->lexer);
    switch (next.type) {
    case TOK_RET:
        return parse_ret(parser);
    case TOK_LBRACE:
        return ITEM(parse_block(parser), block);
    case TOK_SEMI: {
        Token tok = lex_next(parser->lexer);
        return vertex_empty_new(tok.lexeme);
    }
    case TOK_IF:
        return parse_if(parser);
    case TOK_FOR:
    case TOK_WHILE:
    case TOK_DO:
        return parse_loop(parser);
    case TOK_BREAK:
    case TOK_CONTINUE: {
        Item* ret = vertex_break_continue_new(lex_next(parser->lexer).lexeme,
                                              next.type == TOK_BREAK ? STMT_BREAK : STMT_CONTINUE);

        if (!parse_consume(parser, A3_CS("semicolon"), TOK_SEMI))
            return NULL;

        return ret;
    }
    default:
        return parse_expr_stmt(parser);
    }
}

static PType* parse_aggregate_decl(Parser* parser) {
    assert(parser);

    Token s = lex_next(parser->lexer);
    if (s.type != TOK_STRUCT && s.type != TOK_UNION) {
        parse_error(parser, s, "Expected an aggregate declaration.");
        return NULL;
    }

    Span  name     = { .text = A3_CS_NULL, .line = 0 };
    Token tok_name = lex_peek(parser->lexer);
    if (tok_name.type == TOK_IDENT) {
        lex_next(parser->lexer);
        name = tok_name.lexeme;
    }

    PType* ret = ptype_aggregate_new(name.text.ptr ? parse_span_merge(s.lexeme, name) : s.lexeme,
                                     s.type == TOK_STRUCT ? PTY_STRUCT : PTY_UNION, name);

    if (lex_peek(parser->lexer).type != TOK_LBRACE)
        return ret;
    lex_next(parser->lexer);

    while (parse_has_next(parser) && lex_peek(parser->lexer).type != TOK_RBRACE) {
        Items items;
        A3_SLL_INIT(&items);

        if (!parse_decl(parser, &items))
            return NULL;

        A3_SLL_FOR_EACH(Item, item, &items, link) {
            assert(VERTEX(item, item)->type == V_DECL);

            if (item->decl_ptype->is_typedef) {
                error_at(parser->src, SPAN(item, item),
                         "typedef is not permitted inside an aggregate declaration.");
                return NULL;
            }

            Member* member = member_new(item->name, item->decl_ptype);
            free(VERTEX(item, item));

            A3_SLL_ENQUEUE(&ret->members, member, link);
        }
    }

    if (!parse_consume(parser, A3_CS("closing brace"), TOK_RBRACE))
        return NULL;

    return ret;
}

static PTypeBuiltinType parse_decl_flag(Parser* parser, Token tok, PTypeBuiltinType prev) {
    assert(parser);

    PTypeBuiltinType ret = prev;

    switch (tok.type) {
    case TOK_VOID:
        if (ret != PTY_NOTHING) {
            parse_error(parser, tok, "Invalid use of void type.");
            return PTY_NOTHING;
        }
        ret |= PTY_VOID;
        break;
    case TOK_I8:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_I8;
        break;
    case TOK_I16:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_I16;
        break;
    case TOK_I32:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_I32;
        break;
    case TOK_I64:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_I64;
        break;
    case TOK_ISIZE:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_ISIZE;
        break;
    case TOK_U8:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_U8;
        break;
    case TOK_U16:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_U16;
        break;
    case TOK_U32:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_U32;
        break;
    case TOK_U64:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_U64;
        break;
    case TOK_USIZE:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_USIZE;
        break;
    case TOK_CHAR:
        if (ret & (PTY_TYPES | PTY_TYPE_QUALIFIERS)) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_CHAR;
        break;
    case TOK_SHORT:
        if (ret & PTY_TYPE_QUALIFIERS) {
            parse_error(parser, tok, "Duplicate type qualifiers in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_SHORT;
        break;
    case TOK_INT:
        if (ret & PTY_TYPES) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_INT;
        break;
    case TOK_LONG:
        if (ret & PTY_TYPES) {
            parse_error(parser, tok, "Duplicate types in type declaration.");
            return PTY_NOTHING;
        }
        ret += PTY_LONG;
        break;
    case TOK_SIGNED:
        if (ret & (PTY_SIGNED | PTY_UNSIGNED)) {
            parse_error(parser, tok, "Duplicate signed/unsigned qualifier in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_SIGNED;
        break;
    case TOK_UNSIGNED:
        if (ret & (PTY_SIGNED | PTY_UNSIGNED)) {
            parse_error(parser, tok, "Duplicate signed/unsigned qualifier in type declaration.");
            return PTY_NOTHING;
        }
        ret |= PTY_UNSIGNED;
        break;
    default:
        A3_UNREACHABLE();
    }

    return ret;
}

static PType* parse_declspec(Parser* parser) {
    assert(parser);

    if (!parse_has_decl(parser)) {
        parse_error(parser, lex_next(parser->lexer), "Expected a type name.");
        return NULL;
    }

    Token first = lex_peek(parser->lexer);

    bool is_typedef = false;
    if (first.type == TOK_TYPEDEF) {
        lex_next(parser->lexer);
        is_typedef = true;
    }

    if (parse_has_decl_aggregate(parser)) {
        PType* ret = parse_aggregate_decl(parser);
        if (!ret)
            return NULL;
        ret->is_typedef = is_typedef;
        return ret;
    }

    if (parse_has_defined_type(parser)) {
        PType* ret      = ptype_defined_new(lex_next(parser->lexer).lexeme);
        ret->is_typedef = is_typedef;
        return ret;
    }

    Token            next = lex_peek(parser->lexer);
    PTypeBuiltinType type = PTY_NOTHING;
    while (parse_has_decl_builtin(parser)) {
        type = parse_decl_flag(parser, lex_next(parser->lexer), type);
        if (type == PTY_NOTHING)
            return NULL;
    }

    if (is_typedef && type == PTY_NOTHING)
        type = PTY_INT;

    PType* ret      = ptype_builtin_new(parse_span_merge(first.lexeme, next.lexeme), type);
    ret->is_typedef = is_typedef;

    return ret;
}

static PType* parse_decl_suffix_fn(Parser* parser, PType* base) {
    assert(parser);
    assert(base);

    PType* ret = ptype_fn_new(base->span, base);

    if (lex_peek(parser->lexer).type == TOK_VOID) {
        lex_next(parser->lexer);
    } else {
        bool first = true;
        while (parse_has_next(parser) && lex_peek(parser->lexer).type != TOK_RPAREN) {
            if (!first && !parse_consume(parser, A3_CS("comma"), TOK_COMMA))
                return NULL;
            first = false;

            PType* param_type = parse_declspec(parser);
            if (!param_type)
                return NULL;
            if (param_type->is_typedef) {
                error_at(parser->src, param_type->span,
                         "typedef is not permitted in function parameters.");
                return NULL;
            }

            Item* param = parse_declarator(parser, param_type);
            if (!param)
                return NULL;
            assert(VERTEX(param, item)->type == V_DECL);

            A3_SLL_ENQUEUE(&ret->params, param, link);
        }
    }

    Token tok_closing = lex_next(parser->lexer);
    if (tok_closing.type != TOK_RPAREN) {
        parse_error(parser, tok_closing, "Expected a closing parenthesis.");
        return NULL;
    }

    ret->span = parse_span_merge(ret->span, tok_closing.lexeme);

    return ret;
}

static PType* parse_decl_suffix_array(Parser* parser, PType* base) {
    assert(parser);
    assert(base);

    Token next = lex_next(parser->lexer);
    if (next.type != TOK_LIT_NUM) {
        parse_error(parser, next, "Expected a numeric literal.");
        return NULL;
    }

    Token tok_closing = lex_next(parser->lexer);
    if (tok_closing.type != TOK_RBRACKET) {
        parse_error(parser, tok_closing, "Expected a closing bracket.");
        return NULL;
    }

    Token following = lex_peek(parser->lexer);
    if (following.type == TOK_LPAREN || following.type == TOK_LBRACKET)
        base = parse_decl_suffix(parser, base);

    return ptype_array_new(base->span.text.ptr <= tok_closing.lexeme.text.ptr
                               ? parse_span_merge(base->span, tok_closing.lexeme)
                               : base->span,
                           base, (size_t)next.lit_num);
}

static PType* parse_decl_suffix(Parser* parser, PType* base) {
    assert(parser);
    assert(base);

    Token next = lex_next(parser->lexer);
    assert(next.type == TOK_LPAREN || next.type == TOK_LBRACKET);

    if (next.type == TOK_LPAREN)
        return parse_decl_suffix_fn(parser, base);

    return parse_decl_suffix_array(parser, base);
}

static PType* parse_declarator_dummy_replace(PType* nested, PType* base) {
    assert(nested);
    assert(base);

    if (nested->type == PTY_DUMMY)
        return base;

    assert(nested->type == PTY_ARRAY || nested->type == PTY_PTR);
    assert(nested->parent);
    nested->parent = parse_declarator_dummy_replace(nested->parent, base);

    return nested;
}

static Item* parse_declarator(Parser* parser, PType* type) {
    assert(parser);
    assert(type);

    while (lex_peek(parser->lexer).type == TOK_STAR) {
        Token next = lex_next(parser->lexer);
        type       = ptype_ptr_new(parse_span_merge(type->span, next.lexeme), type);
    }

    Token next = lex_peek(parser->lexer);
    if (next.type == TOK_RPAREN || next.type == TOK_COMMA)
        return vertex_decl_new(type->span, A3_CS_NULL, type);

    // Nested declarator.
    Item* nested = NULL;
    PType dummy  = { .type = PTY_DUMMY, .span = type->span };
    if (lex_peek(parser->lexer).type == TOK_LPAREN) {
        lex_next(parser->lexer);

        // Declarators are awful. A nested declarator applies to the outer type, /including
        // suffixes/, so that needs to be parsed in order to establish the base type. This is done
        // by looking up the base chain of the below declaration and replacing PTY_DUMMY with the
        // actual type.
        nested = parse_declarator(parser, &dummy);

        if (!parse_consume(parser, A3_CS("closing parenthesis"), TOK_RPAREN))
            return NULL;
    }

    A3CString name = A3_CS_NULL;
    if (nested) {
        name = nested->name;
    } else {
        Token ident = lex_next(parser->lexer);
        if (ident.type != TOK_IDENT) {
            parse_error(parser, ident, "Expected an identifier.");
            return NULL;
        }

        name = ident.lexeme.text;
    }

    next = lex_peek(parser->lexer);
    if (next.type == TOK_LPAREN || next.type == TOK_LBRACKET)
        type = parse_decl_suffix(parser, type);

    if (!type)
        return NULL;

    if (nested)
        type = parse_declarator_dummy_replace(nested->decl_ptype, type);

    if (type->is_typedef) {
        if (!name.ptr) {
            error_at(parser->src, type->span, "typedef declaration must have a name.");
            return NULL;
        }

        parse_scope_typedef_add(parser, name);
    }

    return vertex_decl_new(type->span, name, type);
}

static bool parse_fn(Parser* parser, Item* decl) {
    assert(parser);
    assert(parser->current_unit);
    assert(decl);
    assert(decl->decl_ptype->type == PTY_FN);

    Block* body = NULL;

    if (lex_peek(parser->lexer).type == TOK_LBRACE) {
        body = parse_block(parser);
        if (!body)
            return false;

        decl->body       = body;
        SPAN(decl, item) = parse_span_merge(SPAN(decl, item), SPAN(body, item.block));
    } else if (!parse_consume(parser, A3_CS("semicolon"), TOK_SEMI)) {
        return false;
    }

    A3_SLL_ENQUEUE(&parser->current_unit->items, decl, link);
    return true;
}

static bool parse_decl(Parser* parser, Items* items) {
    assert(parser);
    assert(items);

    PType* base = parse_declspec(parser);
    if (!base)
        return false;
    assert(base->type == PTY_BUILTIN || base->type == PTY_STRUCT || base->type == PTY_UNION ||
           base->type == PTY_DEFINED);

    bool first = true;
    while (parse_has_next(parser) && lex_peek(parser->lexer).type != TOK_SEMI) {
        if (!first && !parse_consume(parser, A3_CS("comma"), TOK_COMMA))
            return false;
        first = false;

        Item* decl = parse_declarator(parser, base);
        if (!decl)
            return false;

        if (decl->decl_ptype->type == PTY_FN) {
            if (items != &parser->current_unit->items) {
                error_at(parser->src, SPAN(decl, item),
                         "Function declaration not allowed inside block.");
                return false;
            }
            A3_TRYB(parse_fn(parser, decl));

            return true;
        }

        A3_SLL_ENQUEUE(items, decl, link);

        if (lex_peek(parser->lexer).type == TOK_EQ) {
            lex_next(parser->lexer);

            Expr* init = parse_expr(parser, INFIX_PRECEDENCE[TOK_EQ][1]);
            if (!init)
                return false;

            decl->init = init;
        }
    }

    if (first && (base->type == PTY_STRUCT || base->type == PTY_UNION) && base->name.text.ptr) {
        Item* decl = vertex_decl_new(base->name, A3_CS_NULL, base);
        A3_SLL_ENQUEUE(items, decl, link);
    }

    return parse_consume(parser, A3_CS("semicolon"), TOK_SEMI);
}

static bool parse_block_item(Parser* parser, Block* block) {
    assert(parser);
    assert(block);

    if (parse_has_decl(parser))
        return parse_decl(parser, &block->body);

    Item* res = parse_stmt(parser);
    if (!res)
        return false;

    A3_SLL_ENQUEUE(&block->body, res, link);

    return true;
}

static Block* parse_block(Parser* parser) {
    assert(parser);

    parse_scope_push(parser);

    Token left_tok = lex_next(parser->lexer);
    assert(left_tok.type == TOK_LBRACE);

    Block* block = vertex_block_new();

    Token next;
    while ((next = lex_peek(parser->lexer)).type != TOK_RBRACE && next.type != TOK_EOF) {
        if (next.type == TOK_ERR) {
            parser->status = false;

            while (next.type != TOK_SEMI && next.type != TOK_EOF)
                next = lex_next(parser->lexer);
        }

        if (!parse_block_item(parser, block))
            break;
    }
    next = lex_peek(parser->lexer);

    if (next.type == TOK_ERR) {
        block = NULL;
        goto done;
    }

    Token right_tok = lex_next(parser->lexer);
    if (right_tok.type != TOK_RBRACE) {
        parse_error(parser, right_tok, "Expected a closing brace.");
        block = NULL;
        goto done;
    }

    SPAN(block, item.block) = parse_span_merge(left_tok.lexeme, right_tok.lexeme);
done:
    parse_scope_pop(parser);
    return block;
}

Vertex* parse(Parser* parser) {
    assert(parser);

    Unit* unit           = vertex_unit_new();
    parser->current_unit = unit;

    while (parse_has_next(parser)) {
        if (!parse_decl(parser, &parser->current_unit->items)) {
            parser->status = false;
            break;
        }
    }

    Token next = lex_peek(parser->lexer);
    if (parser->status && next.type != TOK_EOF) {
        parse_error(parser, next, "Expected end of file.");
        return NULL;
    }

    if (!parser->status)
        return NULL;

    return VERTEX(unit, unit);
}

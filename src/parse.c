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

#include <a3/str.h>
#include <a3/util.h>

#include "ast.h"
#include "error.h"
#include "lex.h"

typedef struct Parser {
    A3CString src;
    Lexer*    lexer;
    size_t    error_depth;
    bool      status;
} Parser;

static Statement* parse_stmt(Parser*);

Parser* parse_new(A3CString src, Lexer* lexer) {
    A3_UNWRAPNI(Parser*, ret, calloc(1, sizeof(*ret)));
    *ret = (Parser) { .src = src, .lexer = lexer, .error_depth = 0, .status = true };
    return ret;
}

// Report a parser error.
A3_FORMAT_FN(3, 4)
static void parse_error(Parser* parser, Token token, char* fmt, ...) {
    assert(parser);

    parser->status = false;

    va_list args;
    va_start(args, fmt);

    verror_at(parser->src, token.lexeme, fmt, args);

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

static A3CString parse_span_merge(A3CString lhs, A3CString rhs) {
    assert(lhs.ptr);
    assert(rhs.ptr);
    assert(lhs.ptr < rhs.ptr);

    return a3_cstring_new(lhs.ptr, lhs.len + rhs.len + ((size_t)(rhs.ptr - lhs.ptr) - lhs.len));
}

static BinOpType parse_bin_op(OpType type) {
    switch (type) {
    case TOK_OP_PLUS:
        return OP_ADD;
    case TOK_OP_MINUS:
        return OP_SUB;
    case TOK_OP_STAR:
        return OP_MUL;
    case TOK_OP_SLASH:
        return OP_DIV;
    case TOK_OP_EQ_EQ:
        return OP_EQ;
    case TOK_OP_BANG_EQ:
        return OP_NE;
    case TOK_OP_LT:
        return OP_LT;
    case TOK_OP_LT_EQ:
        return OP_LE;
    case TOK_OP_GT:
        return OP_GT;
    case TOK_OP_GT_EQ:
        return OP_GE;
    case TOK_OP_EQ:
        return OP_ASSIGN;
    case TOK_OP_AMP:
        A3_PANIC("TODO");
    case TOK_OP_COUNT:
        A3_UNREACHABLE();
    }

    A3_UNREACHABLE();
}

static UnaryOpType parse_unary_op(OpType type) {
    switch (type) {
    case TOK_OP_PLUS:
        return OP_UNARY_ADD;
    case TOK_OP_MINUS:
        return OP_NEG;
    case TOK_OP_AMP:
        return OP_ADDR;
    case TOK_OP_STAR:
        return OP_DEREF;
    default:
        A3_PANIC("Not a unary operator.");
    }
}

static Expr* parse_var(Parser* parser) {
    assert(parser);

    Token tok = lex_next(parser->lexer);
    assert(tok.type == TOK_IDENT);

    return vertex_var_new(tok.lexeme);
}

static Expr* parse_expr(Parser* parser, uint8_t precedence) {
    assert(parser);

    static uint8_t PREFIX_PRECEDENCE[TOK_OP_COUNT] = {
        [TOK_OP_PLUS]  = 11,
        [TOK_OP_MINUS] = 11,
        [TOK_OP_AMP]   = 11,
        [TOK_OP_STAR]  = 11,
    };

    static uint8_t INFIX_PRECEDENCE[TOK_OP_COUNT][2] = {
        [TOK_OP_EQ] = { 2, 1 },

        [TOK_OP_EQ_EQ] = { 3, 4 }, [TOK_OP_BANG_EQ] = { 3, 4 },

        [TOK_OP_GT] = { 5, 6 },    [TOK_OP_GT_EQ] = { 5, 6 },
        [TOK_OP_LT] = { 5, 6 },    [TOK_OP_LT_EQ] = { 5, 6 },

        [TOK_OP_PLUS] = { 7, 8 },  [TOK_OP_MINUS] = { 7, 8 },

        [TOK_OP_STAR] = { 9, 10 }, [TOK_OP_SLASH] = { 9, 10 }
    };

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
    case TOK_OP:
        if (!PREFIX_PRECEDENCE[tok.op_type]) {
            parse_error(parser, lex_next(parser->lexer), "Expected a unary operator.");
            return NULL;
        }

        lex_next(parser->lexer);
        Expr* rhs = parse_expr(parser, PREFIX_PRECEDENCE[tok.op_type]);
        lhs       = vertex_unary_op_new(tok.lexeme, parse_unary_op(tok.op_type), rhs);
        break;
    case TOK_IDENT:
        lhs = parse_var(parser);
        break;
    default:
        parse_error(parser, lex_next(parser->lexer),
                    "Expected a literal, opening parenthesis, or unary operator.");
        return NULL;
    }

    while (true) {
        Token tok_op = lex_peek(parser->lexer);
        if (tok_op.type != TOK_OP)
            break;

        if (!INFIX_PRECEDENCE[tok_op.op_type][0]) {
            parse_error(parser, lex_next(parser->lexer), "Expected a binary operator.");
            return NULL;
        }

        if (INFIX_PRECEDENCE[tok_op.op_type][0] < precedence)
            break;

        lex_next(parser->lexer);

        Expr* rhs = parse_expr(parser, INFIX_PRECEDENCE[tok_op.op_type][1]);
        lhs       = vertex_bin_op_new(parse_span_merge(SPAN(lhs, expr), SPAN(rhs, expr)),
                                      parse_bin_op(tok_op.op_type), lhs, rhs);
    }

    return lhs;
}

static Statement* parse_expr_stmt(Parser* parser) {
    assert(parser);

    Expr* expr = parse_expr(parser, 0);
    Token next = lex_next(parser->lexer);
    if (next.type != TOK_SEMI) {
        parse_error(parser, next, "Expected a semicolon.");
        return NULL;
    }

    return vertex_expr_stmt_new(parse_span_merge(SPAN(expr, expr), next.lexeme), expr);
}

static Statement* parse_ret(Parser* parser) {
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

static Statement* parse_block(Parser* parser) {
    assert(parser);

    Token left_tok = lex_next(parser->lexer);
    assert(left_tok.type == TOK_LBRACE);

    Block* block = vertex_block_new();

    Statement* current = parse_stmt(parser);
    if (!current)
        return NULL;
    A3_SLL_PUSH(&block->body, current, link);

    Token next = lex_peek(parser->lexer);
    while (next.type != TOK_RBRACE && next.type != TOK_EOF) {
        if (next.type == TOK_ERR) {
            parser->status = false;

            while (next.type != TOK_SEMI && next.type != TOK_EOF)
                next = lex_next(parser->lexer);
        }

        Statement* s = parse_stmt(parser);
        if (!s)
            break;
        A3_SLL_INSERT_AFTER(current, s, link);

        next    = lex_peek(parser->lexer);
        current = s;
    }
    next = lex_peek(parser->lexer);

    if (next.type == TOK_ERR)
        return NULL;

    Token right_tok = lex_next(parser->lexer);
    if (right_tok.type != TOK_RBRACE) {
        parse_error(parser, right_tok, "Expected a closing brace.");
        return NULL;
    }

    SPAN(block, stmt.block) = parse_span_merge(left_tok.lexeme, right_tok.lexeme);
    return STMT(block, block);
}

static Statement* parse_if(Parser* parser) {
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

    Statement* body_true  = parse_stmt(parser);
    Statement* body_false = NULL;

    if (lex_peek(parser->lexer).type == TOK_ELSE) {
        lex_next(parser->lexer);
        body_false = parse_stmt(parser);
    }

    return STMT(vertex_if_new(parse_span_merge(if_tok.lexeme, body_false ? SPAN(body_false, stmt)
                                                                         : SPAN(body_true, stmt)),
                              cond, body_true, body_false),
                if_stmt);
}

static Statement* parse_loop(Parser* parser) {
    assert(parser);

    Token loop_tok = lex_next(parser->lexer);
    assert(loop_tok.type == TOK_FOR || loop_tok.type == TOK_WHILE);

    if (!parse_consume(parser, A3_CS("opening parenthesis"), TOK_LPAREN))
        return NULL;

    Statement* init = NULL;
    Expr*      cond = NULL;
    Expr*      post = NULL;

    if (loop_tok.type == TOK_FOR) {
        init = parse_expr_stmt(parser);
        if (!init)
            return NULL;

        if (lex_peek(parser->lexer).type != TOK_SEMI)
            cond = parse_expr(parser, 0);
        if (!parse_consume(parser, A3_CS("semicolon"), TOK_SEMI))
            return NULL;

        if (lex_peek(parser->lexer).type != TOK_RPAREN)
            post = parse_expr(parser, 0);
    } else {
        cond = parse_expr(parser, 0);
        if (!cond)
            return NULL;
    }

    if (!parse_consume(parser, A3_CS("closing parenthesis"), TOK_RPAREN))
        return NULL;

    Statement* body = parse_stmt(parser);
    if (!body)
        return NULL;

    return STMT(vertex_loop_new(parse_span_merge(loop_tok.lexeme, SPAN(body, stmt)), init, cond,
                                post, body),
                loop);
}

static Statement* parse_stmt(Parser* parser) {
    assert(parser);

    switch (lex_peek(parser->lexer).type) {
    case TOK_RET:
        return parse_ret(parser);
    case TOK_LBRACE:
        return parse_block(parser);
    case TOK_SEMI: {
        Token tok = lex_next(parser->lexer);
        return vertex_empty_new(tok.lexeme);
    }
    case TOK_IF:
        return parse_if(parser);
    case TOK_FOR:
    case TOK_WHILE:
        return parse_loop(parser);
    default:
        return parse_expr_stmt(parser);
    }
}

Vertex* parse(Parser* parser) {
    assert(parser);

    Statement* body = parse_block(parser);
    if (!body)
        return NULL;
    assert(body->type == STMT_BLOCK);

    Token next = lex_peek(parser->lexer);
    if (next.type != TOK_EOF) {
        parse_error(parser, next, "Expected end of file.");
        return NULL;
    }

    if (!parser->status)
        return NULL;

    return VERTEX(vertex_fn_new(A3_CS("main"), &body->block), fn);
}

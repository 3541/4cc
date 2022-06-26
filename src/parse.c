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

static Vertex* parse_recover(Parser* parser) {
    if (parser->error_depth++ >= PARSE_ERRORS_MAX)
        return NULL;

    while (lex_peek(parser->lexer).type != TOK_LIT_NUM)
        lex_next(parser->lexer);

    return parse(parser);
}

static A3CString parse_span_merge(A3CString lhs, A3CString rhs) {
    assert(lhs.ptr);
    assert(rhs.ptr);
    assert(lhs.ptr < rhs.ptr);

    return a3_cstring_new(lhs.ptr, lhs.len + rhs.len + ((size_t)(rhs.ptr - lhs.ptr) - lhs.len));
}

static BinOpType parse_bin_op(OpType type) {
    switch (type) {
    case OP_PLUS:
        return OP_ADD;
    case OP_MINUS:
        return OP_SUB;
    case OP_STAR:
        return OP_MUL;
    case OP_SLASH:
        return OP_DIV;
        break;
    case OP_COUNT:
        A3_UNREACHABLE();
    }

    A3_UNREACHABLE();
}

static UnaryOpType parse_unary_op(OpType type) {
    switch (type) {
    case OP_PLUS:
        return OP_UNARY_ADD;
    case OP_MINUS:
        return OP_NEG;
    default:
        A3_PANIC("Not a unary operator.");
    }
}

static Vertex* parse_expr(Parser* parser, uint8_t precedence) {
    assert(parser);

    static uint8_t PREFIX_PRECEDENCE[OP_COUNT] = {
        [OP_PLUS]  = 5,
        [OP_MINUS] = 5,
    };

    static uint8_t INFIX_PRECEDENCE[OP_COUNT][2] = {
        [OP_PLUS]  = { 1, 2 },
        [OP_MINUS] = { 1, 2 },
        [OP_STAR]  = { 3, 4 },
        [OP_SLASH] = { 3, 4 },
    };

    Vertex* lhs = NULL;
    Token   tok = lex_peek(parser->lexer);
    switch (tok.type) {
    case TOK_LPAREN:
        lex_next(parser->lexer);
        lhs = parse_expr(parser, 0);
        if (!parse_consume(parser, A3_CS("closing parenthesis"), TOK_RPAREN))
            return parse_recover(parser);
        break;
    case TOK_LIT_NUM: {
        lex_next(parser->lexer);
        lhs = vertex_lit_num_new(tok.lexeme, tok.lit_num);
        break;
    }
    case TOK_OP:
        if (!PREFIX_PRECEDENCE[tok.op_type]) {
            parse_error(parser, lex_next(parser->lexer), "Expected a unary operator.");
            return parse_recover(parser);
        }

        lex_next(parser->lexer);
        Vertex* rhs = parse_expr(parser, PREFIX_PRECEDENCE[tok.op_type]);
        lhs         = vertex_unary_op_new(tok.lexeme, parse_unary_op(tok.op_type), rhs);
        break;
    default:
        parse_error(parser, lex_next(parser->lexer),
                    "Expected a literal, opening parenthesis, or unary operator.");
        return parse_recover(parser);
    }

    while (true) {
        Token tok_op = lex_peek(parser->lexer);
        if (tok_op.type != TOK_OP)
            break;

        if (INFIX_PRECEDENCE[tok_op.op_type][0] < precedence)
            break;

        lex_next(parser->lexer);

        Vertex* rhs = parse_expr(parser, INFIX_PRECEDENCE[tok_op.op_type][1]);
        lhs         = vertex_bin_op_new(parse_span_merge(lhs->span, rhs->span),
                                        parse_bin_op(tok_op.op_type), lhs, rhs);
    }

    return lhs;
}

Vertex* parse(Parser* parser) {
    assert(parser);

    Vertex* ret = parse_expr(parser, 0);

    if (!parser->status)
        return NULL;

    return ret;
}

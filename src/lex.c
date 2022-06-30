/*
 * LEXER -- Source file tokenization.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include "lex.h"

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include <a3/buffer.h>
#include <a3/str.h>
#include <a3/util.h>

#include "error.h"

typedef struct Lexer {
    Token    peek;
    bool     peeking;
    A3Buffer src;
    size_t   error_depth;
} Lexer;

// Report a lexer error.
A3_FORMAT_FN(2, 3)
static void lex_error(Lexer const* lexer, char* fmt, ...) {
    assert(lexer);

    va_list args;
    va_start(args, fmt);

    A3CString highlight = a3_buf_read_ptr(&lexer->src);
    highlight.len       = 1;
    verror_at(A3_S_CONST(lexer->src.data), highlight, fmt, args);

    va_end(args);
}

Lexer* lex_new(A3CString src) {
    A3_UNWRAPNI(Lexer*, ret, calloc(1, sizeof(*ret)));

    *ret = (Lexer) { .peeking     = false,
                     .src         = { .data = A3_CS_MUT(src), .tail = a3_string_len(src) },
                     .error_depth = 0 };
    a3_buf_init(&ret->src, a3_string_len(src), a3_string_len(src));
    return ret;
}

void lex_free(Lexer* lexer) {
    assert(lexer);

    free(lexer);
}

bool lex_is_eof(Lexer const* lexer) {
    assert(lexer);

    return !a3_buf_len(&lexer->src);
}

bool lex_failed(Lexer const* lexer) {
    assert(lexer);

    return lexer->error_depth;
}

static uint8_t lex_peek_byte(Lexer const* lexer) {
    assert(lexer);
    assert(!lex_is_eof(lexer));

    return *a3_string_cptr(a3_buf_read_ptr(&lexer->src));
}

static A3CString lex_peek_str(Lexer const* lexer) {
    assert(lexer);

    return a3_buf_read_ptr(&lexer->src);
}

static void lex_consume_any(Lexer* lexer, size_t n) {
    assert(lexer);
    assert(!lex_is_eof(lexer));
    assert(n <= a3_buf_len(&lexer->src));

    a3_buf_read(&lexer->src, n);
}

static A3CString lex_consume_one(Lexer* lexer, A3CString name, A3CString delim) {
    assert(lexer);

    uint8_t next = lex_peek_byte(lexer);
    if (!a3_string_cptr(a3_string_rchr(delim, next))) {
        lex_error(lexer, "Expected a " A3_S_F ".", A3_S_FORMAT(name));
        return A3_CS_NULL;
    }

    A3CString ret = a3_cstring_new(a3_string_cptr(a3_buf_read_ptr(&lexer->src)), 1);
    lex_consume_any(lexer, 1);
    return ret;
}

static A3CString lex_consume_until(Lexer* lexer, bool (*pred)(uint8_t)) {
    assert(lexer);

    if (lex_is_eof(lexer))
        return A3_CS_NULL;

    A3CString ret = lex_peek_str(lexer);
    ret.len       = 0;

    while (!lex_is_eof(lexer) && !pred(lex_peek_byte(lexer))) {
        lex_consume_any(lexer, 1);
        ret.len++;
    }

    return ret;
}

static bool is_digit(uint8_t c) { return isdigit(c); }

static bool is_not_space(uint8_t c) { return !isspace(c); }

static bool is_ident_first(uint8_t c) { return isalpha(c) || c == '_'; }

static bool is_ident(uint8_t c) { return is_ident_first(c) || is_digit(c); }

static bool is_not_ident(uint8_t c) { return !is_ident(c); }

static void lex_consume_space(Lexer* lexer) {
    assert(lexer);

    lex_consume_until(lexer, is_not_space);
}

static Token lex_recover(Lexer* lexer) {
    assert(lexer);

    lex_consume_until(lexer, is_digit);
    return (Token) { .type = TOK_ERR };
    return lex_next(lexer);
}

static Token lex_lit_num(Lexer* lexer) {
    assert(lexer);
    assert(isdigit(lex_peek_byte(lexer)));

    int       offset = -1;
    int64_t   num    = -1;
    A3CString s      = lex_peek_str(lexer);
    if (sscanf(a3_string_cstr(s), "%" SCNd64 "%n", &num, &offset) < 1) {
        s.len = 1;
        lex_error(lexer, "Expected a numeric literal.");
        return lex_recover(lexer);
    }
    assert(offset > 0);

    lex_consume_any(lexer, (size_t)offset);
    return (Token) { .type    = TOK_LIT_NUM,
                     .lexeme  = a3_cstring_new(s.ptr, (size_t)offset),
                     .lit_num = num };
}

static Token lex_op(Lexer* lexer) {
    assert(lexer);

    A3CString lexeme = lex_consume_one(lexer, A3_CS("binary operator"), A3_CS("+-*/=!<>&"));
    if (!a3_string_cptr(lexeme))
        return lex_recover(lexer);

    TokenType type;
    switch (lexeme.ptr[0]) {
    case '+':
        type = TOK_PLUS;
        break;
    case '-':
        type = TOK_MINUS;
        break;
    case '*':
        type = TOK_STAR;
        break;
    case '/':
        type = TOK_SLASH;
        break;
    case '&':
        type = TOK_AMP;
        break;
    case '=':
        if (lexeme.ptr[1] != '=') {
            type = TOK_EQ;
            break;
        }

        lexeme.len++;
        lex_consume_any(lexer, 1);
        type = TOK_EQ_EQ;
        break;
    case '!':
        if (!lex_consume_one(lexer, A3_CS("relational inequality"), A3_CS("=")).ptr)
            return lex_recover(lexer);

        lexeme.len++;
        lex_consume_any(lexer, 1);
        type = TOK_BANG_EQ;
        break;
    case '<':
        if (lexeme.ptr[1] != '=') {
            type = TOK_LT;
            break;
        }

        lexeme.len++;
        lex_consume_any(lexer, 1);
        type = TOK_LT_EQ;
        break;
    case '>':
        if (lexeme.ptr[1] != '=') {
            type = TOK_GT;
            break;
        }

        lexeme.len++;
        lex_consume_any(lexer, 1);
        type = TOK_GT_EQ;
        break;
    default:
        A3_UNREACHABLE();
    }

    return (Token) { .type = type, .lexeme = lexeme };
}

static Token lex_paren(Lexer* lexer) {
    assert(lexer);

    A3CString lexeme = lex_consume_one(lexer, A3_CS("parenthesis or brace"), A3_CS("(){}"));
    if (!a3_string_cptr(lexeme))
        return lex_recover(lexer);

    switch (a3_string_cptr(lexeme)[0]) {
    case '(':
        return (Token) { .type = TOK_LPAREN, .lexeme = lexeme };
    case ')':
        return (Token) { .type = TOK_RPAREN, .lexeme = lexeme };
    case '{':
        return (Token) { .type = TOK_LBRACE, .lexeme = lexeme };
    case '}':
        return (Token) { .type = TOK_RBRACE, .lexeme = lexeme };
    default:
        A3_UNREACHABLE();
    }
}

static Token lex_semi(Lexer* lexer) {
    assert(lexer);

    A3CString lexeme = lex_consume_one(lexer, A3_CS("semicolon"), A3_CS(";"));
    if (!a3_string_cptr(lexeme))
        return lex_recover(lexer);

    return (Token) { .type = TOK_SEMI, .lexeme = lexeme };
}

static Token lex_ident_or_kw(Lexer* lexer) {
    assert(lexer);

    static struct {
        A3CString name;
        TokenType type;
    } KEYWORDS[] = { { A3_CS("return"), TOK_RET },  { A3_CS("if"), TOK_IF },
                     { A3_CS("else"), TOK_ELSE },   { A3_CS("for"), TOK_FOR },
                     { A3_CS("while"), TOK_WHILE }, { A3_CS("int"), TOK_INT },
                     { A3_CS("void"), TOK_VOID } };

    A3CString lexeme = lex_consume_until(lexer, is_not_ident);
    if (!a3_string_cptr(lexeme))
        return lex_recover(lexer);

    for (size_t i = 0; i < sizeof(KEYWORDS) / sizeof(KEYWORDS[0]); i++) {
        if (a3_string_cmp(lexeme, KEYWORDS[i].name) == 0)
            return (Token) { .type = KEYWORDS[i].type, .lexeme = lexeme };
    }

    return (Token) { .type = TOK_IDENT, .lexeme = lexeme };
}

Token lex_peek(Lexer* lexer) {
    assert(lexer);

    if (lexer->peeking)
        return lexer->peek;

    if (lexer->error_depth++ >= LEX_ERRORS_MAX)
        return (Token) { .type = TOK_EOF, .lexeme = A3_CS_NULL };

    lexer->peeking = true;

    lex_consume_space(lexer);

    if (lex_is_eof(lexer)) {
        lexer->peek = (Token) { .type = TOK_EOF, .lexeme = A3_CS_NULL };
        return lexer->peek;
    }

    uint8_t next = lex_peek_byte(lexer);
    switch (next) {
    case '+':
    case '-':
    case '*':
    case '/':
    case '<':
    case '>':
    case '=':
    case '!':
    case '&':
        lexer->peek = lex_op(lexer);
        break;
    case '(':
    case ')':
    case '{':
    case '}':
        lexer->peek = lex_paren(lexer);
        break;
    case ';':
        lexer->peek = lex_semi(lexer);
        break;
    case ',':
        lexer->peek = (Token) { .type   = TOK_COMMA,
                                .lexeme = lex_consume_one(lexer, A3_CS("comma"), A3_CS(",")) };
        break;
    default:
        if (is_digit(next)) {
            lexer->peek = lex_lit_num(lexer);
            break;
        }
        if (is_ident_first(next)) {
            lexer->peek = lex_ident_or_kw(lexer);
            break;
        }

        lex_error(lexer, "Expected a numeric literal, binary operator, keyword, or identifier.");
        return lex_recover(lexer);
    }

    return lexer->peek;
}

Token lex_next(Lexer* lexer) {
    Token ret      = lex_peek(lexer);
    lexer->peeking = false;

    return ret;
}

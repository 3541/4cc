/*
 * LEXER -- Source file tokenization.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <a3/str.h>

#include "ast.h"
#include "error.h"

typedef enum TokenType {
    TOK_AMP,
    TOK_AMP_AMP,
    TOK_AMP_EQ,
    TOK_BANG,
    TOK_BANG_EQ,
    TOK_BREAK,
    TOK_CASE,
    TOK_CHAR,
    TOK_COLON,
    TOK_COMMA,
    TOK_CONST,
    TOK_CONTINUE,
    TOK_DEFAULT,
    TOK_DO,
    TOK_DOT,
    TOK_DOT_DOT_DOT,
    TOK_ELSE,
    TOK_ENUM,
    TOK_EQ,
    TOK_EQ_EQ,
    TOK_EXTERN,
    TOK_FOR,
    TOK_GOTO,
    TOK_GT,
    TOK_GT_EQ,
    TOK_GT_GT,
    TOK_GT_GT_EQ,
    TOK_HAT,
    TOK_HAT_EQ,
    TOK_I16,
    TOK_I32,
    TOK_I64,
    TOK_I8,
    TOK_IDENT,
    TOK_IF,
    TOK_INT,
    TOK_ISIZE,
    TOK_LBRACE,
    TOK_LBRACKET,
    TOK_LIT_NUM,
    TOK_LIT_STR,
    TOK_LONG,
    TOK_LPAREN,
    TOK_LT,
    TOK_LT_EQ,
    TOK_LT_LT,
    TOK_LT_LT_EQ,
    TOK_MINUS,
    TOK_MINUS_EQ,
    TOK_MINUS_GT,
    TOK_MINUS_MINUS,
    TOK_PERCENT,
    TOK_PERCENT_EQ,
    TOK_PIPE,
    TOK_PIPE_EQ,
    TOK_PIPE_PIPE,
    TOK_PLUS,
    TOK_PLUS_EQ,
    TOK_PLUS_PLUS,
    TOK_QUERY,
    TOK_RBRACE,
    TOK_RBRACKET,
    TOK_RET,
    TOK_RPAREN,
    TOK_SEMI,
    TOK_SHORT,
    TOK_SIGNED,
    TOK_SIZEOF,
    TOK_SLASH,
    TOK_SLASH_EQ,
    TOK_STAR,
    TOK_STAR_EQ,
    TOK_STATIC,
    TOK_STRUCT,
    TOK_SWITCH,
    TOK_TILDE,
    TOK_TYPEDEF,
    TOK_U16,
    TOK_U32,
    TOK_U64,
    TOK_U8,
    TOK_UNION,
    TOK_UNSIGNED,
    TOK_USIZE,
    TOK_VOID,
    TOK_VOLATILE,
    TOK_WHILE,

    TOK_EOF,
    TOK_ERR,

    TOK_COUNT
} TokenType;

typedef enum {
    LIT_NUM_UNSIGNED  = 1,
    LIT_NUM_CHAR      = 1 << 1,
    LIT_NUM_LONG      = 1 << 2,
    LIT_NUM_LONG_LONG = 1 << 3,
    LIT_NUM_SIZE      = 1 << 4,
    LIT_NUM_FLOAT     = 1 << 5,
    LIT_NUM_DOUBLE    = 1 << 6,

    LIT_NUM_INT_MASK =
        LIT_NUM_UNSIGNED | LIT_NUM_CHAR | LIT_NUM_SIZE | LIT_NUM_LONG | LIT_NUM_LONG_LONG,
    LIT_NUM_FLOAT_ONLY_MASK = LIT_NUM_FLOAT | LIT_NUM_DOUBLE,
    LIT_NUM_FLOAT_MASK      = LIT_NUM_FLOAT_ONLY_MASK | LIT_NUM_LONG,
} LitNumType;

typedef struct LitNum {
    LitNumType type;

    union {
        uintmax_t   integer;
        long double fp;
    };
} LitNum;

typedef union {
    LitNum    num;
    A3CString str;
} TokLit;

typedef struct {
    TokenType type;
    Span      lexeme;

    TokLit lit; // TOK_LIT_NUM, TOK_LIT_STR.
} Token;

typedef struct Lexer Lexer;

Lexer* lex_new(A3CString src);
void   lex_free(Lexer*);
bool   lex_is_eof(Lexer const*);
Token  lex_peek_n(Lexer*, size_t);
Token  lex_peek(Lexer*);
Token  lex_next(Lexer*);

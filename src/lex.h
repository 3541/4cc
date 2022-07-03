/*
 * LEXER -- Source file tokenization.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#pragma once

#include <stdint.h>

#include <a3/str.h>

#define LEX_ERRORS_MAX 512

typedef enum TokenType {
    TOK_AMP,
    TOK_AMP_AMP,
    TOK_BANG,
    TOK_BANG_EQ,
    TOK_CHAR,
    TOK_COMMA,
    TOK_ELSE,
    TOK_EQ,
    TOK_EQ_EQ,
    TOK_FOR,
    TOK_GT,
    TOK_GT_EQ,
    TOK_IDENT,
    TOK_IF,
    TOK_INT,
    TOK_LBRACE,
    TOK_LBRACKET,
    TOK_LIT_NUM,
    TOK_LIT_STR,
    TOK_LPAREN,
    TOK_LT,
    TOK_LT_EQ,
    TOK_MINUS,
    TOK_PIPE_PIPE,
    TOK_PLUS,
    TOK_RBRACE,
    TOK_RBRACKET,
    TOK_RET,
    TOK_RPAREN,
    TOK_SEMI,
    TOK_SLASH,
    TOK_STAR,
    TOK_TILDE,
    TOK_VOID,
    TOK_WHILE,

    TOK_EOF,
    TOK_ERR,

    TOK_COUNT
} TokenType;

typedef struct Token {
    TokenType type;
    A3CString lexeme;

    union {
        int64_t   lit_num;
        A3CString lit_str;
    };
} Token;

typedef struct Lexer Lexer;

Lexer* lex_new(A3CString src);
void   lex_free(Lexer*);
bool   lex_is_eof(Lexer const*);
bool   lex_failed(Lexer const*);
Token  lex_peek(Lexer*);
Token  lex_next(Lexer*);

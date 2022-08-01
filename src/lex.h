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

#include "error.h"

#define LEX_ERRORS_MAX 512

typedef enum TokenType {
    TOK_AMP,
    TOK_AMP_AMP,
    TOK_AMP_EQ,
    TOK_BANG,
    TOK_BANG_EQ,
    TOK_BREAK,
    TOK_CHAR,
    TOK_COLON,
    TOK_COMMA,
    TOK_CONST,
    TOK_CONTINUE,
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
    TOK_LIT_CHAR,
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

typedef struct Token {
    TokenType type;
    Span      lexeme;

    union {
        A3CString lit_str;
        uint8_t   lit_char;

        struct {
            uintmax_t lit_num;
            bool      lit_num_is_signed;
        };
    };
} Token;

typedef struct Lexer Lexer;

Lexer* lex_new(A3CString src);
void   lex_free(Lexer*);
bool   lex_is_eof(Lexer const*);
bool   lex_failed(Lexer const*);
Token  lex_peek_n(Lexer*, size_t);
Token  lex_peek(Lexer*);
Token  lex_next(Lexer*);

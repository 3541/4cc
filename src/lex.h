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

typedef enum TokenType { TOK_OP, TOK_LPAREN, TOK_RPAREN, TOK_LIT_NUM, TOK_EOF, TOK_ERR } TokenType;
typedef enum OpType { OP_PLUS, OP_MINUS, OP_STAR, OP_SLASH, OP_COUNT } OpType;

typedef struct Token {
    TokenType type;
    A3CString lexeme;

    union {
        int64_t lit_num;
        OpType  op_type;
    };
} Token;

typedef struct Lexer Lexer;

Lexer* lex_new(A3CString src);
void   lex_free(Lexer*);
bool   lex_is_eof(Lexer const*);
bool   lex_failed(Lexer const*);
Token  lex_peek(Lexer*);
Token  lex_next(Lexer*);

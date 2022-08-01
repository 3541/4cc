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
#include <a3/sll.h>
#include <a3/str.h>
#include <a3/util.h>

#include "error.h"

typedef struct Peek Peek;
typedef struct Peek {
    A3_SLL_LINK(Peek) link;
    Token tok;
} Peek;

typedef struct Lexer {
    A3_SLL(peek, Peek) peek;
    A3Buffer           src;
    size_t             current_line;
    size_t             error_depth;
} Lexer;

// Report a lexer error.
A3_FORMAT_FN(2, 3)
static void lex_error(Lexer const* lexer, char* fmt, ...) {
    assert(lexer);

    va_list args;
    va_start(args, fmt);

    Span span     = { .text = a3_buf_read_ptr(&lexer->src), .line = lexer->current_line };
    span.text.len = 1;
    verror_at(A3_S_CONST(lexer->src.data), span, fmt, args);

    va_end(args);
}

Lexer* lex_new(A3CString src) {
    A3_UNWRAPNI(Lexer*, ret, calloc(1, sizeof(*ret)));

    *ret = (Lexer) { .src          = { .data = A3_CS_MUT(src), .tail = a3_string_len(src) },
                     .current_line = 1,
                     .error_depth  = 0 };
    a3_buf_init(&ret->src, a3_string_len(src), a3_string_len(src));
    A3_SLL_INIT(&ret->peek);
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
        if (lex_peek_byte(lexer) == '\n')
            lexer->current_line++;
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

static bool is_newline(uint8_t c) { return c == '\n' || c == '\r'; };

static bool is_star(uint8_t c) { return c == '*'; }

static void lex_consume_space(Lexer* lexer) {
    assert(lexer);

    while (true && !lex_is_eof(lexer)) {
        if (isspace(lex_peek_byte(lexer))) {
            lex_consume_until(lexer, is_not_space);
            continue;
        }

        A3CString first_two = lex_peek_str(lexer);
        first_two.len       = MIN(first_two.len, 2);
        if (a3_string_cmp(first_two, A3_CS("//")) == 0) {
            lex_consume_until(lexer, is_newline);
            continue;
        }
        if (a3_string_cmp(first_two, A3_CS("/*")) == 0) {
            while (true && !lex_is_eof(lexer)) {
                lex_consume_until(lexer, is_star);
                lex_consume_any(lexer, 1);
                if (lex_peek_byte(lexer) == '/') {
                    lex_consume_any(lexer, 1);
                    break;
                }
            }

            continue;
        }

        break;
    }
}

static Token tok_new(Lexer const* lexer, TokenType type, A3CString lexeme) {
    return (Token) { .type = type, .lexeme = { .line = lexer->current_line, .text = lexeme } };
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
    uintmax_t num    = 0;
    A3CString s      = lex_peek_str(lexer);

    int res = -1;
    if (s.ptr[0] == '0' && tolower(s.ptr[1]) == 'x')
        res = sscanf(a3_string_cstr(s), "%" SCNxMAX "%n", &num, &offset);
    else if (s.ptr[0] == '0')
        res = sscanf(a3_string_cstr(s), "%" SCNoMAX "%n", &num, &offset);
    else
        res = sscanf(a3_string_cstr(s), "%" SCNuMAX "%n", &num, &offset);

    if (res < 1) {
        s.len = 1;
        lex_error(lexer, "Expected a numeric literal.");
        return lex_recover(lexer);
    }
    assert(offset > 0);

    lex_consume_any(lexer, (size_t)offset);

    bool is_signed = false;
    while (strchr("ul", tolower(lex_peek_byte(lexer)))) {
        if (tolower(lex_peek_byte(lexer)) == 'u')
            is_signed = true;

        lex_consume_any(lexer, 1);
    }

    Token ret             = tok_new(lexer, TOK_LIT_NUM, a3_cstring_new(s.ptr, (size_t)offset));
    ret.lit_num_is_signed = is_signed;
    ret.lit_num           = num;

    return ret;
}

static Token lex_op(Lexer* lexer) {
    assert(lexer);

    A3CString lexeme =
        lex_consume_one(lexer, A3_CS("unary or binary operator"), A3_CS("+-*/=!<>&~|.?:%^"));
    if (!a3_string_cptr(lexeme))
        return lex_recover(lexer);

    TokenType type;
    switch (lexeme.ptr[0]) {
    case '~':
        type = TOK_TILDE;
        break;
    case '?':
        type = TOK_QUERY;
        break;
    case ':':
        type = TOK_COLON;
        break;
    case '%':
        if (lexeme.ptr[1] != '=') {
            type = TOK_PERCENT;
            break;
        }

        lexeme.len++;
        lex_consume_any(lexer, 1);
        type = TOK_PERCENT_EQ;
        break;
    case '/':
        if (lexeme.ptr[1] != '=') {
            type = TOK_SLASH;
            break;
        }

        lexeme.len++;
        lex_consume_any(lexer, 1);
        type = TOK_SLASH_EQ;
        break;
    case '*':
        if (lexeme.ptr[1] != '=') {
            type = TOK_STAR;
            break;
        }

        lexeme.len++;
        lex_consume_any(lexer, 1);
        type = TOK_STAR_EQ;
        break;
    case '+':
        switch (lexeme.ptr[1]) {
        case '+':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_PLUS_PLUS;
            break;
        case '=':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_PLUS_EQ;
            break;
        default:
            type = TOK_PLUS;
            break;
        }
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
        if (lexeme.ptr[1] != '=') {
            type = TOK_BANG;
            break;
        }

        lexeme.len++;
        lex_consume_any(lexer, 1);
        type = TOK_BANG_EQ;
        break;
    case '<':
        switch (lexeme.ptr[1]) {
        case '=':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_LT_EQ;
            break;
        case '<':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_LT_LT;

            if (lexeme.ptr[2] == '=') {
                lexeme.len++;
                lex_consume_any(lexer, 1);
                type = TOK_LT_LT_EQ;
            }
            break;
        default:
            type = TOK_LT;
            break;
        }
        break;
    case '>':
        switch (lexeme.ptr[1]) {
        case '=':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_GT_EQ;
            break;
        case '>':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_GT_GT;

            if (lexeme.ptr[2] == '=') {
                lexeme.len++;
                lex_consume_any(lexer, 1);
                type = TOK_GT_GT_EQ;
            }
            break;
        default:
            type = TOK_GT;
            break;
        }
        break;
    case '&':
        switch (lexeme.ptr[1]) {
        case '&':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_AMP_AMP;
            break;
        case '=':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_AMP_EQ;
            break;
        default:
            type = TOK_AMP;
            break;
        }
        break;
    case '|':
        switch (lexeme.ptr[1]) {
        case '|':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_PIPE_PIPE;
            break;
        case '=':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_PIPE_EQ;
            break;
        default:
            type = TOK_PIPE;
            break;
        }
        break;
    case '-':
        switch (lexeme.ptr[1]) {
        case '>':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_MINUS_GT;
            break;
        case '-':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_MINUS_MINUS;
            break;
        case '=':
            lexeme.len++;
            lex_consume_any(lexer, 1);
            type = TOK_MINUS_EQ;
            break;
        default:
            type = TOK_MINUS;
            break;
        }
        break;
    case '.':
        if (lexeme.ptr[1] != '.' || lexeme.ptr[2] != '.') {
            type = TOK_DOT;
            break;
        }

        lexeme.len += 2;
        lex_consume_any(lexer, 2);
        type = TOK_DOT_DOT_DOT;
        break;
    case '^':
        if (lexeme.ptr[1] != '=') {
            type = TOK_HAT;
            break;
        }

        lexeme.len++;
        lex_consume_any(lexer, 1);
        type = TOK_HAT_EQ;
        break;
    default:
        A3_UNREACHABLE();
    }

    return tok_new(lexer, type, lexeme);
}

static Token lex_paren(Lexer* lexer) {
    assert(lexer);

    A3CString lexeme = lex_consume_one(lexer, A3_CS("parenthesis or brace"), A3_CS("(){}[]"));
    if (!a3_string_cptr(lexeme))
        return lex_recover(lexer);

    switch (a3_string_cptr(lexeme)[0]) {
    case '(':
        return tok_new(lexer, TOK_LPAREN, lexeme);
    case ')':
        return tok_new(lexer, TOK_RPAREN, lexeme);
    case '{':
        return tok_new(lexer, TOK_LBRACE, lexeme);
    case '}':
        return tok_new(lexer, TOK_RBRACE, lexeme);
    case '[':
        return tok_new(lexer, TOK_LBRACKET, lexeme);
    case ']':
        return tok_new(lexer, TOK_RBRACKET, lexeme);
    default:
        A3_UNREACHABLE();
    }
}

static Token lex_semi(Lexer* lexer) {
    assert(lexer);

    A3CString lexeme = lex_consume_one(lexer, A3_CS("semicolon"), A3_CS(";"));
    if (!a3_string_cptr(lexeme))
        return lex_recover(lexer);

    return tok_new(lexer, TOK_SEMI, lexeme);
}

static Token lex_ident_or_kw(Lexer* lexer) {
    assert(lexer);

    static struct {
        A3CString name;
        TokenType type;
    } const KEYWORDS[] = {
        { A3_CS("return"), TOK_RET },        { A3_CS("if"), TOK_IF },
        { A3_CS("else"), TOK_ELSE },         { A3_CS("for"), TOK_FOR },
        { A3_CS("while"), TOK_WHILE },       { A3_CS("int"), TOK_INT },
        { A3_CS("void"), TOK_VOID },         { A3_CS("char"), TOK_CHAR },
        { A3_CS("sizeof"), TOK_SIZEOF },     { A3_CS("struct"), TOK_STRUCT },
        { A3_CS("union"), TOK_UNION },       { A3_CS("break"), TOK_BREAK },
        { A3_CS("continue"), TOK_CONTINUE }, { A3_CS("do"), TOK_DO },
        { A3_CS("__i8"), TOK_I8 },           { A3_CS("__i16"), TOK_I16 },
        { A3_CS("__i32"), TOK_I32 },         { A3_CS("__i64"), TOK_I64 },
        { A3_CS("__isize"), TOK_ISIZE },     { A3_CS("__u8"), TOK_U8 },
        { A3_CS("__u16"), TOK_U16 },         { A3_CS("__u32"), TOK_U32 },
        { A3_CS("__u64"), TOK_U64 },         { A3_CS("__usize"), TOK_USIZE },
        { A3_CS("short"), TOK_SHORT },       { A3_CS("long"), TOK_LONG },
        { A3_CS("typedef"), TOK_TYPEDEF },   { A3_CS("unsigned"), TOK_UNSIGNED },
        { A3_CS("signed"), TOK_SIGNED },     { A3_CS("extern"), TOK_EXTERN },
        { A3_CS("const"), TOK_CONST },       { A3_CS("enum"), TOK_ENUM },
        { A3_CS("static"), TOK_STATIC },     { A3_CS("volatile"), TOK_VOLATILE },
        { A3_CS("goto"), TOK_GOTO },         { A3_CS("switch"), TOK_SWITCH },
        { A3_CS("case"), TOK_CASE },         { A3_CS("default"), TOK_DEFAULT },
    };

    A3CString lexeme = lex_consume_until(lexer, is_not_ident);
    if (!a3_string_cptr(lexeme))
        return lex_recover(lexer);

    for (size_t i = 0; i < sizeof(KEYWORDS) / sizeof(KEYWORDS[0]); i++) {
        if (a3_string_cmp(lexeme, KEYWORDS[i].name) == 0)
            return tok_new(lexer, KEYWORDS[i].type, lexeme);
    }

    return tok_new(lexer, TOK_IDENT, lexeme);
}

static size_t lex_escape(A3CString e, uint8_t* o) {
    switch (*e.ptr) {
    case '\'':
        *o = '\'';
        return 1;
    case '"':
        *o = '"';
        return 1;
    case '?':
        *o = '?';
        return 1;
    case '\\':
        *o = '\\';
        return 1;
    case 'a':
        *o = '\a';
        return 1;
    case 'b':
        *o = '\b';
        return 1;
    case 'e':
        *o = '\e';
        return 1;
    case 'f':
        *o = '\f';
        return 1;
    case 'n':
        *o = '\n';
        return 1;
    case 'r':
        *o = '\r';
        return 1;
    case 't':
        *o = '\t';
        return 1;
    case 'v':
        *o = '\v';
        return 1;
    case 'x': {
        size_t  i   = 1;
        uint8_t num = 0;
        for (; i < MIN(3, e.len) && isxdigit(e.ptr[i]); i++) {
            num *= 16;

            uint8_t c = (uint8_t)tolower(e.ptr[i]);
            if ('a' <= c && c <= 'f')
                num += (uint8_t)(c - 'a' + 10);
            else
                num += (uint8_t)(c - '0');
        }

        *o = num;
        return i;
    }
    default:
        if (is_digit(*e.ptr) && *e.ptr < '8') {
            uint16_t num = 0;

            size_t i = 0;
            for (; i < MIN(3, e.len) && is_digit(e.ptr[i]) && e.ptr[i] < '8'; i++) {
                num *= 8;
                num += (uint16_t)(e.ptr[i] - '0');
            }

            if (num > UINT8_MAX)
                return 0;

            *o = (uint8_t)num;
            return i;
        }

        return 0;
    }
}

static Token lex_lit_str(Lexer* lexer) {
    assert(lexer);
    assert(lex_peek_byte(lexer) == '"');

    A3CString s   = lex_peek_str(lexer);
    A3Buffer* buf = a3_buf_new(32, 1024);

    for (size_t i = 1; i < s.len; i++) {
        switch (s.ptr[i]) {
        case '"':
            s.len = i + 1;
            break;
        case '\\': {
            uint8_t c;
            i++;
            size_t len;
            if (i >= s.len ||
                !(len = lex_escape(A3_S_CONST(a3_string_offset(A3_CS_MUT(s), i)), &c))) {
                lex_error(lexer, "Invalid escape sequence.");
                return lex_recover(lexer);
            }

            a3_buf_write_byte(buf, c);
            i += len - 1;
            break;
        }
        default:
            a3_buf_write_byte(buf, s.ptr[i]);
        }
    }

    if (s.ptr[s.len - 1] != '"') {
        lex_error(lexer, "Bad string literal.");
        return lex_recover(lexer);
    }

    lex_consume_any(lexer, s.len);

    Token ret   = tok_new(lexer, TOK_LIT_STR, s);
    ret.lit_str = a3_buf_read_ptr(buf);
    return ret;
}

static Token lex_lit_char(Lexer* lexer) {
    assert(lexer);
    assert(lex_peek_byte(lexer) == '\'');

    A3CString s   = lex_peek_str(lexer);
    uint8_t   c   = s.ptr[1];
    size_t    len = 3;

    if (c == '\\') {
        if (!(len = lex_escape(A3_S_CONST(a3_string_offset(A3_CS_MUT(s), 2)), &c))) {
            lex_error(lexer, "Invalid escape sequence.");
            return lex_recover(lexer);
        }

        len += 3;
    }

    if (s.len < len || s.ptr[len - 1] != '\'') {
        lex_error(lexer, "Bad character literal.");
        return lex_recover(lexer);
    }
    s.len = len;

    lex_consume_any(lexer, s.len);

    Token ret    = tok_new(lexer, TOK_LIT_CHAR, s);
    ret.lit_char = c;
    return ret;
}

static void lex_tok_enqueue(Lexer* lexer, Token tok) {
    assert(lexer);

    A3_UNWRAPNI(Peek*, peek, calloc(1, sizeof(*peek)));
    peek->tok = tok;

    A3_SLL_ENQUEUE(&lexer->peek, peek, link);
}

Token lex_peek_n(Lexer* lexer, size_t n) {
    assert(lexer);
    assert(n > 0);

    if (lexer->error_depth >= LEX_ERRORS_MAX)
        return tok_new(lexer, TOK_EOF, A3_CS_NULL);

    Peek*  ret = A3_SLL_HEAD(&lexer->peek);
    size_t i   = 1;
    for (; i < n && ret && ret->tok.type != TOK_EOF; i++)
        ret = A3_SLL_NEXT(ret, link);

    if (ret)
        return ret->tok;

    lex_consume_space(lexer);

    Token tok;
    for (; i <= n && !lex_is_eof(lexer) && lexer->error_depth <= LEX_ERRORS_MAX; i++) {
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
        case '~':
        case '|':
        case '.':
        case '?':
        case ':':
        case '%':
        case '^':
            tok = lex_op(lexer);
            break;
        case '(':
        case ')':
        case '{':
        case '}':
        case '[':
        case ']':
            tok = lex_paren(lexer);
            break;
        case ';':
            tok = lex_semi(lexer);
            break;
        case ',':
            tok = tok_new(lexer, TOK_COMMA, lex_consume_one(lexer, A3_CS("comma"), A3_CS(",")));
            break;
        case '"':
            tok = lex_lit_str(lexer);
            break;
        case '\'':
            tok = lex_lit_char(lexer);
            break;
        default:
            if (is_digit(next)) {
                tok = lex_lit_num(lexer);
                break;
            }
            if (is_ident_first(next)) {
                tok = lex_ident_or_kw(lexer);
                break;
            }

            lex_error(lexer,
                      "Expected a numeric literal, binary operator, keyword, or identifier.");
            return lex_recover(lexer);
        }

        lex_tok_enqueue(lexer, tok);
    }

    if (lexer->error_depth >= LEX_ERRORS_MAX)
        return tok_new(lexer, TOK_EOF, A3_CS_NULL);

    if (lex_is_eof(lexer)) {
        lex_tok_enqueue(lexer, tok_new(lexer, TOK_EOF, A3_CS_NULL));
        tok = A3_SLL_HEAD(&lexer->peek)->tok;
    }

    return tok;
}

Token lex_peek(Lexer* lexer) {
    assert(lexer);

    return lex_peek_n(lexer, 1);
}

Token lex_next(Lexer* lexer) {
    Token ret = lex_peek(lexer);
    A3_SLL_DEQUEUE(&lexer->peek, link);

    return ret;
}

/*
 * 4CC -- C compiler.
 *
 * Copyright (c) 2022, Alex O'Brien <3541@3541.website>
 *
 * This file is licensed under the BSD 3-clause license. See the LICENSE file in the project root
 * for details.
 */

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <a3/str.h>

#include "ast.h"
#include "error.h"
#include "gen.h"
#include "lex.h"
#include "parse.h"

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <EXPR>\n", argv[0]);
        return -1;
    }

    A3CString input  = a3_cstring_from(argv[1]);
    Lexer*    lexer  = lex_new(input);
    Parser*   parser = parse_new(input, lexer);

    Vertex* root = parse(parser);
    if (!root)
        return -1;

    if (!gen(input, root))
        return -1;

    return 0;
}

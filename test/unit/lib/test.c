#include "test.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

int return_42(void) { return 42; }
int return_1(void) { return 1; }
int sum2(int a, int b) { return a + b; }
int sum6(int a1, int a2, int a3, int a4, int a5, int a6) { return a1 + a2 + a3 + a4 + a5 + a6; }
int sub3(int a, int b, int c) { return a + b - c; }

int GLOBAL = 28;

int return_43(void) { return 43; }

size_t variadic_sum(size_t c, ...) {
    va_list args;
    va_start(args, c);

    size_t ret = 0;
    for (size_t i = 0; i < c; i++)
        ret += va_arg(args, size_t);

    va_end(args);

    return ret;
}

void test_assert_eq(size_t expr, size_t expected, char* expr_str, int line) {
    if (expr != expected) {
        fprintf(stderr, "Assertion failure (%d). Expected %s == %zu, but got %zu.\n", line,
                expr_str, expected, expr);
        exit(-1);
    }
}

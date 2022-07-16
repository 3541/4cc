#pragma once

#include <stddef.h>

int return_42(void);
int return_1(void);
int sum2(int, int);
int sum6(int, int, int, int, int, int);
int sub3(int, int, int);

int puts(char const*);

void test_assert_eq(size_t, size_t, char*, int);

size_t variadic_sum(size_t c, ...);

#define ASSERT_EQ(EXPR, EXPECTED) test_assert_eq((EXPR), (EXPECTED), #EXPR, __LINE__)

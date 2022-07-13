#pragma once

int return_42(void);
int return_1(void);
int sum2(int, int);
int sum6(int, int, int, int, int, int);
int sub3(int, int, int);

#ifndef REAL_COMPILER
int puts(char*);
#define size_t __i64
#else
#include <stddef.h>
#endif

int test_assert_eq(size_t, size_t, char*, int);

#define ASSERT_EQ(EXPR, EXPECTED) test_assert_eq((EXPR), (EXPECTED), #EXPR, __LINE__)

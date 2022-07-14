#include "test.h"

extern int return_43();

int main(void) { ASSERT_EQ(return_43(), 43); }

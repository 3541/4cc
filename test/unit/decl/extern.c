#include "test.h"

extern int GLOBAL;

int main(void) { ASSERT_EQ(GLOBAL, 28); }

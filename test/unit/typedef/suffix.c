#include "test.h"

int main(void) {
    typedef int A[4];

    A x;
    x[2] = 4;

    ASSERT_EQ(sizeof(x), 16);
    ASSERT_EQ(x[2], 4);
}

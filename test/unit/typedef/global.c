#include "test.h"

typedef int T, A[2];
T           g;

int main(void) {
    T x;
    A a;

    a[0] = 2;
    g    = 4;

    ASSERT_EQ(sizeof(x), 4);
    ASSERT_EQ(sizeof(a), 8);
    ASSERT_EQ(g, 4);
}

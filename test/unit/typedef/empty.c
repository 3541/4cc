#include "test.h"

int main(void) {
    typedef T;

    T x = 8;

    ASSERT_EQ(sizeof(x), 4);
    ASSERT_EQ(x, 8);
}

#include "test.h"

int main(void) {
    typedef int T;
    T           x = 4;

    ASSERT_EQ(sizeof(x), 4);
    ASSERT_EQ(x, 4);
}

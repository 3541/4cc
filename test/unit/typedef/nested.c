#include "test.h"

int main(void) {
    typedef int T;
    typedef T   U;

    U x;

    ASSERT_EQ(sizeof(x), 4);
}

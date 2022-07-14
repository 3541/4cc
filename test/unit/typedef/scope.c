#include "test.h"

int main(void) {
    typedef short T;
    { typedef int T; }
    T x;

    ASSERT_EQ(sizeof(x), 2);
}

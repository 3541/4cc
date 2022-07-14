#include "test.h"

int main(void) {
    signed __u8    x;
    signed __usize y;
    signed char    z;

    ASSERT_EQ(sizeof(x), 1);
    ASSERT_EQ(sizeof(y), 8);
    ASSERT_EQ(sizeof(z), 1);
}

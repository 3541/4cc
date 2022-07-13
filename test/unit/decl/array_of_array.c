#include "test.h"

int main(void) {
    long a[4][5];
    long(b[4])[5];

    ASSERT_EQ(sizeof(a), sizeof(b));
    ASSERT_EQ(sizeof(a), 160);
    ASSERT_EQ(sizeof(a[0]), 40);
}

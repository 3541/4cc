#include "test.h"

int main(void) {
    unsigned int  x;
    unsigned long y;

    ASSERT_EQ(sizeof(x), 4);
    ASSERT_EQ(sizeof(y), 8);
}

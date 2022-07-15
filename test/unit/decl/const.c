#include "test.h"

int main(void) {
    const int x = 4;

    ASSERT_EQ(sizeof(x), 4);
    ASSERT_EQ(x, 4);
}

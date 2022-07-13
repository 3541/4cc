#include "test.h"

int main(void) {
    char(a)[4];
    int b[8];

    ASSERT_EQ(sizeof(a), 4);
    ASSERT_EQ(sizeof(b), 32);
}

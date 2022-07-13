#include "test.h"

int main(void) {
    int* a[5];

    ASSERT_EQ(sizeof(a), 40);
}

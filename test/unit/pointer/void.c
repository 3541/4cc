#include "test.h"

int main(void) {
    int   x = 23;
    void* p = &x;
    int*  y = p;

    ASSERT_EQ(*y, 23);
}

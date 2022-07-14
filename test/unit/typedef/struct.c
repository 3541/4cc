#include "test.h"

int main(void) {
    typedef struct {
        int x;
    } T;
    T x;
    x.x = 31;

    ASSERT_EQ(sizeof(x), 4);
    ASSERT_EQ(x.x, 31);
}

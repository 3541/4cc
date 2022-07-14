#include "test.h"

int main(void) {
    long          x;
    long int      y;
    long long     z;
    long long int w;

    short     a;
    short int b;

    ASSERT_EQ(sizeof(x), 8);
    ASSERT_EQ(sizeof(y), 8);
    ASSERT_EQ(sizeof(z), 8);
    ASSERT_EQ(sizeof(w), 8);

    ASSERT_EQ(sizeof(a), 2);
    ASSERT_EQ(sizeof(b), 2);
}

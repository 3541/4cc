#include "test.h"

struct S;

void below(void);

int main(void) {
    struct S* x;

    ASSERT_EQ(sizeof(x), 8);
    below();
}

struct S {
    int a;
    int b;
    int c;
};

void below(void) { ASSERT_EQ(sizeof(struct S), 12); }

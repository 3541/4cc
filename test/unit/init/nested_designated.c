#include <assert.h>

struct S {
    int a[4];
};

struct T {
    struct S s[2];
};

struct U {
    struct T t;
};

int main(void) {
    struct U u[2] = { [1].t.s[1].a[3] = 23 };

    assert(u[1].t.s[1].a[2] == 0);
    assert(u[1].t.s[1].a[3] == 23);
}

#include <assert.h>

struct S {
    int  x;
    int  y;
    long z;
};

struct T {
    int      x;
    struct S s1;
    short    y;
    struct S s2;
};

int main(void) {
    struct T t = { 20, { .x = 1, 2, .z = 3 }, .y = 4, .s2 = { 4, 5, 6 } };

    assert(t.x == 20);
    assert(t.s1.x == 1);
    assert(t.s1.y == 2);
    assert(t.s1.z == 3);
    assert(t.y == 4);
    assert(t.s2.x == 4);
    assert(t.s2.y == 5);
    assert(t.s2.z == 6);
}

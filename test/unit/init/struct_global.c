#include <assert.h>
#include <stddef.h>
#include <stdint.h>

struct S {
    int32_t  x;
    uint64_t y;
    int      a[20];
    char     c;
} s = { -42, 8192, { 4, 5, 6, 7 }, 'h' };

int main(void) {
    assert(s.x == -42);
    assert(s.y == 8192);
    assert(s.a[0] == 4);
    assert(s.a[1] == 5);
    assert(s.a[2] == 6);
    assert(s.a[3] == 7);

    for (size_t i = 4; i < 20; i++)
        assert(s.a[i] == 0);

    assert(s.c == 'h');
}

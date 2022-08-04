#include <assert.h>
#include <stdint.h>

struct S {
    uint32_t x;
    char     c;
    int      y;
};

int main(void) {
    struct S s = { 42, 'a' };

    assert(s.x == 42);
    assert(s.c == 'a');
    assert(s.y == 0);
}

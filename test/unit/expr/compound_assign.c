#include <assert.h>
#include <stddef.h>

int main(void) {
    size_t x = 3;

    assert((x += 2) == 5);
    assert(x == 5);

    assert((x -= 4) == 1);
    assert(x == 1);

    assert((x *= 80) == 80);
    assert(x == 80);

    assert((x /= 2) == 40);
    assert(x == 40);

    assert((x %= 3) == 1);
    assert(x == 1);

    assert((x <<= 20) == 1048576);
    assert(x == 1048576);

    assert((x >>= 15) == 32);
    assert(x == 32);

    assert((x &= 3) == 0);
    assert(x == 0);

    assert((x |= 7) == 7);
    assert(x == 7);

    assert((x ^= 3) == 4);
    assert(x == 4);
}

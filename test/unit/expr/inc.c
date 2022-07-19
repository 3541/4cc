#include <assert.h>
#include <stddef.h>

int main(void) {
    size_t x = 24;

    assert(x == 24);
    assert(++x == 25);
    assert(x == 25);
    assert(x++ == 25);
    assert(x == 26);
}

#include <assert.h>
#include <stddef.h>

int main(void) {
    size_t x = 24;

    assert(x == 24);
    assert(--x == 23);
    assert(x == 23);
    assert(x-- == 23);
    assert(x == 22);
}

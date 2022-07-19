#include <assert.h>

int main(void) {
    assert(1 << 0 == 1);
    assert(1 << 1 == 2);
    assert(1 << 8 == 256);
    assert(9 << 2 == 36);
    assert(0 << 8 == 0);
}

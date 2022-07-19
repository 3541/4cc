#include <assert.h>
#include <stdio.h>

int main(void) {
    assert((0 & 1) == 0);
    assert((3 & 1) == 1);
    assert((7 & 3) == 3);
    assert((-1 & 10) == 10);

    // C operator precedence...
    assert(!(0 & 1 == 0)); // This parses as 0 & (1 == 0).
}

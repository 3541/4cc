#include <assert.h>

int main(void) {
    assert(0 ^ 0 == 0);
    assert(0 ^ 1 == 1);
    assert(1 ^ 1 == 0);
    assert(170 ^ 85 == 255);
    assert(255 ^ 85 == 170);
}

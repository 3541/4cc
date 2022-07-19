#include <assert.h>

int main(void) {
    assert(0 | 1 == 1);
    assert(1 | 1 == 1);
    assert(0 | 0 == 0);
    assert(2 | 1 == 3);
}

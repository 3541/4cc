#include <assert.h>

int main(void) {
    assert(-1 >> 1 == -1);
    assert(8 >> 2 == 2);
    assert(24 >> 3 == 3);
}

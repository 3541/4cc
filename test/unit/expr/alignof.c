#include <assert.h>

int main(void) {
    assert(_Alignof(char) == 1);
    assert(alignof(200) == 4);
    assert(alignof(__usize) == 8);
}

#include <assert.h>
#include <stddef.h>

size_t choose(size_t x) {
    switch (x) {
    case 0:
        return 42;
    default:
        return 0;
    }
}

int main(void) {
    assert(choose(0) == 42);
    assert(choose(1) == 0);
    assert(choose(2) == 0);
    assert(choose(3) == 0);
    assert(choose(80000) == 0);
}

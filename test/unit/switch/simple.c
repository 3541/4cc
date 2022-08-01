#include <assert.h>
#include <stddef.h>

size_t choose(size_t x) {
    switch (x) {
    case 0:
        return 42;
    case 1:
        return 43;
    case 2:
        break;
    case 3:
        return 45;
    }

    return 44;
}

int main(void) {
    assert(choose(0) == 42);
    assert(choose(1) == 43);
    assert(choose(2) == 44);
    assert(choose(3) == 45);
}

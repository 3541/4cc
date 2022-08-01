#include <assert.h>
#include <stddef.h>

int main(void) {
    size_t x = 2;

    switch (8) {
    case 0:
        x = 22;
        break;
    case 8:
        x = 44;
    case 9:
        x *= 2;
        break;
    }

    assert(x == 88);
}

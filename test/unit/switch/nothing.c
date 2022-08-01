#include <assert.h>

int main(void) {
    int x = 0;

    switch (-1) {
    case 0:
        x = 2;
        break;
    case 1:
        x = 4;
        break;
    case 9:
        x = 99;
        break;
    }

    assert(!x);
}

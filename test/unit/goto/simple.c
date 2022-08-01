#include <assert.h>

int test(int x) {
    int ret = 1;

    if (x == 0)
        goto a;
    if (x == 1)
        goto b;
    if (x == 2)
        goto c;

a:
    ret *= 2;
b:
    ret *= 2;
c:
    ret *= 2;

    return ret;
}

int main(void) {
    assert(test(0) == 8);
    assert(test(1) == 4);
    assert(test(2) == 2);
}

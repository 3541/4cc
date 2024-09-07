#include <assert.h>
#include <stddef.h>

int main(void) {
    int a[] = { 1, 2, 3, [8] = 4, 9 };

    assert(a[0] == 1);
    assert(a[1] == 2);
    assert(a[2] == 3);

    for (size_t i = 3; i < 8; ++i)
        assert(a[i] == 0);

    assert(a[8] == 4);
    assert(a[9] == 9);
}

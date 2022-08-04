#include <assert.h>
#include <stdint.h>
#include <stdio.h>

struct S {
    char     a;
    uint64_t as[8];
    uint32_t b;
};

int main(void) {
    struct S s = { 'a', { 1, 2, 3, 4, 5, 6, 7, 8 }, 9 };

    assert(s.a == 'a');

    for (uint64_t i = 1; i <= 8; i++)
        assert(s.as[i - 1] == i);

    assert(s.b == 9);
}

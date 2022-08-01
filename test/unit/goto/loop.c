#include <assert.h>

int main(void) {
    unsigned i = 0;

loop:
    if (++i < 200)
        goto loop;

    assert(i == 200);
}

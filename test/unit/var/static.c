#include <assert.h>
#include <stddef.h>

size_t fn(void) {
    static size_t x = 5;

    return x++;
}

int main(void) {
    for (size_t i = 5; i < 1000; i++)
        assert(fn() == i);
}

#include <assert.h>
#include <stddef.h>

size_t f(size_t x) { return 42 * x; }

int main(void) {
    size_t (*f_ptr)(size_t) = f;

    assert(f_ptr(2) == 84);
}

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>

size_t sum_all(size_t n, ...) {
    va_list args;
    va_start(args, n);

    size_t ret = 0;
    for (size_t i = 0; i < n; i++)
        ret += va_arg(args, size_t);

    return ret;
}

int main(void) {
    assert(sum_all(4, 2, 2, 2, 2) == 8);
    assert(sum_all(1, 1) == 1);
    assert(sum_all(5, 1, 2, 3, 4, 5) == 15);
}

#include <assert.h>
#include <stddef.h>

size_t fn(size_t a1, size_t a2, size_t a3, size_t a4, size_t a5, size_t a6, size_t a7, size_t a8,
          size_t a9, size_t a10) {
    return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10;
}

int main(void) { assert(fn(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == 55); }

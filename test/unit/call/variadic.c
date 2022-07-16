#include <assert.h>

#include "test.h"

int main(void) {
    assert(variadic_sum(2, 3, 3) == 6);
    assert(variadic_sum(5, 2, 3, 8, 9, 10) == 32);
}

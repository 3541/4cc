#include <assert.h>
#include <stddef.h>

size_t fn(size_t x) { return x * 8; }

size_t (*get_fn(void))(size_t) { return fn; }

int main(void) {
    size_t (*fn_ptr)(size_t) = get_fn();
    assert(fn_ptr(2) == 16);

    assert(get_fn()(32) == 256);
}

#include <assert.h>
#include <stddef.h>

int main(void) {
    char t[15 * sizeof(int) - 4 * sizeof(void*) - sizeof(size_t)];
    assert(sizeof(t) == 20);
}

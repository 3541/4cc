#include <assert.h>
#include <stdbool.h>

void f(int x) {
    if (x < 5)
        return;

    assert(false);
}

int main(void) {
    f(4);
}

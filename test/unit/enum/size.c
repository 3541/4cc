#include <assert.h>

int main(void) {
    enum E { A, B, C };

    assert(sizeof(enum E) == 4);
}

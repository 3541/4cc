#include <assert.h>

int main(void) {
    enum { A, B, C, D, E };

    assert(A == 0);
    assert(B == 1);
    assert(C == 2);
    assert(D == 3);
    assert(E == 4);
}

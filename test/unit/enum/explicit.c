#include <assert.h>

int main(void) {
    enum E { A, B = 42, C, D, E = 31, F, G = -1, H };

    assert(A == 0);
    assert(B == 42);
    assert(C == 43);
    assert(D == 44);
    assert(E == 31);
    assert(F == 32);
    assert(G == -1);
    assert(H == 0);
}

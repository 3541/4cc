#include <assert.h>

int main(void) {
    enum E { A, B, C, D };
    typedef enum E T;

    enum E a = B;
    T      b = C;

    assert(a == 1);
    assert(b == 2);
}

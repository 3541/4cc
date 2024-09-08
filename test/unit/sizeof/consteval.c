#include <assert.h>

int main(void) {
    long x;
    enum E { A = sizeof(int), B = sizeof(x) };
    assert(A == sizeof(int));
    assert(B == sizeof(x));
}

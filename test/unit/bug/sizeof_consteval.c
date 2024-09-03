#include <assert.h>

int main(void) {
    enum E { A = sizeof(int) };
    assert(A == sizeof(int));
}

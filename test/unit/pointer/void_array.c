#include <assert.h>

int main(void) {
    int   a[] = { 1, 2, 3, 4 };
    void* p   = a;
    int*  x   = p;

    assert(*x == 1);
}

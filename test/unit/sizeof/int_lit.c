#include <assert.h>

int main(void) {
    assert(sizeof(1234) == sizeof(int));
    assert(sizeof(1234U) == sizeof(int));
    assert(sizeof(1234L) == sizeof(long));
    assert(sizeof(1234LL) == sizeof(long long));
}

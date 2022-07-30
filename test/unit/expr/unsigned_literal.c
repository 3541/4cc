#include <assert.h>

int main(void) {
    assert(1234u == 1234U);
    assert(1234u == 1234);
}

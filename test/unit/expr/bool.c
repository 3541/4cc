#include <assert.h>

int main(void) {
    _Bool b = true;
    bool  c = false;

    assert(b);
    assert(!c);
}

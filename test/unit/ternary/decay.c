#include <assert.h>

int main(void) {
    char const* a = false ? "no" : "yes";
    assert(a[0] == 'y');
}

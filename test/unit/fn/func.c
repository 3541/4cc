#include <assert.h>

int main(void) {
    char const* name = __func__;

    assert(name[0] == 'm');
    assert(name[1] == 'a');
    assert(name[2] == 'i');
    assert(name[3] == 'n');
    assert(name[4] == '\0');
}

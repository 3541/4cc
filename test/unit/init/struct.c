#include <assert.h>
#include <stdint.h>

struct S {
    int     a;
    int64_t b;
    char    c;
};

int main(void) {
    struct S s = { -200, 123456, 'a' };

    assert(s.a == -200);
    assert(s.b == 123456);
    assert(s.c == 'a');
}

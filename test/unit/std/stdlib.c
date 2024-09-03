#include <assert.h>
#include <stdlib.h>

struct S {
    int  x;
    long y;
};

int main(void) {
    struct S* s = malloc(sizeof(struct S));
    s->x        = 2;
    s->y        = 4;

    assert(s->x == 2);
    assert(s->y == 4);
}

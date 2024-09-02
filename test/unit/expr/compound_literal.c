#include <assert.h>
#include <stdio.h>

struct S {
    int x;
};

struct S* f(struct S* s) {
    s->x += 4;
    return s;
}

int main(void) {
    struct S* s = f(&(struct S) { 2 });
    assert(s->x == 6);

    struct S* t = f(&(struct S) { 8 });
    assert(t->x == 12);
    assert(s->x == 6);
}

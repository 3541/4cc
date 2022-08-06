#include <assert.h>

struct S {
    int a;
};

/* clang-format off

Error (718):
    ((x.a += 1) ? (void) (0) : __assert_fail ("x.a &= 1", "/home/alex/src/4cc/test/unit/bug/field_name_corruption.c", 12, ((const char *) 0)));
      ^~~ No member named Ô exists.
Typechecking failed.

clang-format on */

int main(void) {
    struct S x = { 1 };
    assert(x.a += 1);
}

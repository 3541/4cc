int main(void) {
    struct S {
        int a;
        int b;
    } x;
    x.a = 2;

    struct S* y = &x;
    y->b        = 5;

    return y->a + x.b;
}

struct S {
    int a;
    int b;
};

int main(void) {
    struct S x;
    x.a = 3;
    x.b = 4;

    struct S y = x;

    return y.a + y.b;
}

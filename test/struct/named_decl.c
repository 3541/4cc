int main(void) {
    struct S {
        int a;
        int b;
    };

    struct S y;
    y.a = 2;
    y.b = 3;

    return y.a + y.b;
}

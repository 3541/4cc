int main(void) {
    struct {
        int a;
        int b;
    } x;
    x.a = 3;
    x.b = 4;

    return x.a + x.b;
}

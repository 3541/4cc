int main(void) {
    struct {
        int a, b;
    } x;
    x.a = x.b = 0;

    return x.a + x.b;
}

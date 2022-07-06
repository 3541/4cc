int main(void) {
    struct {
        char a;
        int  b;
        char c;
        int  d;
    } x;
    x.a = 8;
    x.b = 7;
    x.c = 2;
    x.d = 20;

    return x.a + x.b * x.c + x.d;
}

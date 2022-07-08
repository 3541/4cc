int main(void) {
    union {
        struct {
            char a[16];
            int  b;
        } s1;
        struct {
            char a[32];
        } s2;
    } x;

    x.s2.a[15] = 8;
    return x.s1.a[15];
}

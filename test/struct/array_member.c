int main(void) {
    struct {
        char a[40];
        char b[20];
    } x;

    char* p = x.a;
    p[2]    = 4;
    p[45]   = 5;

    return x.a[2] + x.b[5];
}

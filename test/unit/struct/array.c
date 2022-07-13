int main(void) {
    struct {
        int  a;
        char b;
        char c;
    } x[10];
    int* p = &x[8].a;
    *p     = 31;

    return x[8].a;
}

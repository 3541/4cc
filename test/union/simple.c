int main(void) {
    union {
        int  i;
        char c;
    } x;

    x.i = 298;
    return x.c;
}

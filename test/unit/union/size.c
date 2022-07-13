int main(void) {
    union {
        int  i;
        char c;
        int  a[16];
    } x;

    return sizeof(x);
}

int main(void) {
    int x     = 123;
    int y     = 234;
    *(&x - 1) = 132;
    return y;
}

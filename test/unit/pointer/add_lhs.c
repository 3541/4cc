int main(void) {
    int x     = 0;
    int y     = 99;
    *(&y + 1) = 240;
    return x;
}

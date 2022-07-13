int main(void) {
    int  x = 0;
    int* y = &x;
    *y     = 2;
    return x;
}

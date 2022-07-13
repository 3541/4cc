int main(void) {
    int   x = 42;
    int*  y = &x;
    int** z = &y;
    return **z;
}

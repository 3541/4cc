int main(void) {
    int x = 0;
    int y = 42;
    return *(&x - 1);
}

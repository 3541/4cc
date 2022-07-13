int main(void) {
    int x = 42;
    { int x = 36; }
    return x;
}

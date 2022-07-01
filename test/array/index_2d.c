int main(void) { int a[2][3]; a[0][0] = 42; a[1][1] = 36; return **a + *(*(a + 1) + 1); }

int main(void) { int x[3]; *x = 1; *(x + 1) = 2; *(x + 2) = 3; return *x + *(x + 1) + *(x + 2); }

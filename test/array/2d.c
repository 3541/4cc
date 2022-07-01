int main(void) { int x[2][3]; int* r = *x; *r = 42; return **x; }

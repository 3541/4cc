int fn1(void) { return 8; }
int fn2(void) { return 9; }
int fn3(void) { return fn1() * fn2(); }
int main(void) { return fn3(); }

#include <assert.h>

int f(int, int x, int) { return x; }
int main(void) { assert(f(1, 2, 3) == 2); }

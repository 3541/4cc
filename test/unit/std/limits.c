#include <assert.h>
#include <limits.h>

int main(void) {
    long long x = LLONG_MAX;
    long long y = LLONG_MIN;

    assert(x > 200);
    assert(y < 200);
}

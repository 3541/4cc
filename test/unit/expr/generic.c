#include <assert.h>

int main() {
    int x;
    assert(_Generic(x, int: 1, short: 0));

    assert(_Generic(x, short: 0, long: 0, default: 1));
}

#include <assert.h>
#include <time.h>

int main(void) {
    time_t t = time(NULL);

    assert(t > 0);
}

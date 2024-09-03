#include <assert.h>
#include <errno.h>

int main(void) {
    errno = 23;
    assert(errno == 23);
}

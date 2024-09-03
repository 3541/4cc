#include <assert.h>
#include <signal.h>

int CALLED = 0;

void handle(int) { CALLED = 1; }

int main(void) {
    void (*prev)(int) = signal(SIGINT, handle);
    assert(prev != SIG_ERR);

    int res = raise(SIGINT);
    assert(res == 0);
    assert(CALLED);
}

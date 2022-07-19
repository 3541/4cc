#include <assert.h>

static int static_fn(void) { return 23; }

int main(void) { assert(static_fn() == 23); }

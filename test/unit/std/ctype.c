#include <assert.h>
#include <ctype.h>
#include <stddef.h>

int main(void) {
    char const* test = "abc123defxyz";

    for (size_t i = 0; i < sizeof("abc123defxyz") - 1; i = i + 1)
        assert(isalnum(test[i]));

    assert(!isalnum("-"[0]));
}

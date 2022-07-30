#include <assert.h>
#include <string.h>

int main(void) {
    assert(strcmp("abcdef", "ab"
                            "cd"
                            "ef") == 0);
}

#include <assert.h>
#include <string.h>

int main(void) {
    char* a = "Greetings.";
    char* b = "Greetings.";
    char* c = "Something else.";

    assert(strcmp(a, b) == 0);
    assert(strcmp(b, c) != 0);
}

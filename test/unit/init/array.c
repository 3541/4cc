#include <assert.h>
#include <stddef.h>

int GLOBAL[] = { 1, 2, 3, 4 };

int main(void) {
    size_t a[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    for (size_t i = 0; i <= 10; i++)
        assert(a[i] == i);

    char s[]  = "Greetings";
    char sa[] = { 'G', 'r', 'e', 'e', 't', 'i', 'n', 'g', 's', '\0' };
    assert(sizeof(s) == sizeof(sa));
    assert(sizeof(s) == sizeof("Greetings"));
    assert(sizeof(s) > sizeof(void*));

    for (size_t i = 0; i < sizeof(s); i++)
        assert(s[i] == sa[i]);

    char* ssa[] = { "An", "array", "of", "strings." };
    assert(sizeof(ssa) == 4 * sizeof(char*));
    assert(ssa[2][0] == 'o');

    assert(GLOBAL[3] == 4);
}

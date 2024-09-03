#include <assert.h>
#include <locale.h>
#include <stddef.h>
#include <string.h>

int main(void) {
    char const* l = setlocale(LC_ALL, NULL);
    assert(strcmp(l, "C") == 0);
}

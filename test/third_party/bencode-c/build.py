from common import build_and_test

build_and_test(
    url = "https://github.com/skeeto/bencode-c.git",
    rev = "4da64ee4b830b386579af6618018067ad392aea1",
    # Default, minus -fsanitize.
    cflags = "-ansi -pedantic -Wall -Wextra -Wno-missing-field-initializers -O3 -ggdb3",
    test_target = "check"
)

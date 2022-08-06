import os
import sys
from common import build_and_test

build_and_test(
    dir = os.path.dirname(sys.argv[0]),
    cc = os.path.realpath(sys.argv[1]),
    url = "https://github.com/skeeto/bencode-c.git",
    rev = "4da64ee4b830b386579af6618018067ad392aea1",
    # Default, minus -fsanitize.
    cflags = "-ansi -pedantic -Wall -Wextra -Wno-missing-field-initializers -O3 -ggdb3",
    test_target = "check"
)

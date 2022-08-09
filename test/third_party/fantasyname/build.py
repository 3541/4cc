from common import build_and_test

build_and_test(
    url = "https://github.com/skeeto/fantasyname.git",
    rev = "e5a475287902457dac09284fb681c13dfdd96715",
    subdir = "c",
    # No -fpie.
    cflags = "-ansi -pedantic -Wall -Wextra -O3 -g3",
    ldflags = "-no-pie"
)

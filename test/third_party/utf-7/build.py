import os
import sys
from common import build_and_test

build_and_test(
    dir = os.path.dirname(sys.argv[0]),
    cc = os.path.realpath(sys.argv[1]),
    url = "https://github.com/skeeto/utf-7.git",
    rev = "4376317190ac799ab5334d23cc403dbd6861ba8d",
    test_target = "check"
)

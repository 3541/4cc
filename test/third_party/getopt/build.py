import os
import sys
from common import build_and_test

build_and_test(
    dir = os.path.dirname(sys.argv[0]),
    cc = os.path.realpath(sys.argv[1]),
    url = "https://github.com/skeeto/getopt.git",
    rev = "55d8fefe680d9b7e68ab80eb46e1bd4ad324fc29"
)

#!/usr/bin/env sh
#
# This script is intended to be called from a CI pipeline (either directly or from all.sh). It
# assumes the library has already been built in the given directory, and runs the tests.

. ci/common.sh

dir="$1"

if [ ! -d "$dir" ]; then
    echo "Build directory $dir does not exist." >&2
    exit 1
fi

if ! command -v python3 > /dev/null 2>&1; then
    echo "python3 not found. Skipping tests."
    exit
fi

if [ "$(python3 --version | cut -d'.' -f2)" -lt 6 ]; then
    echo "$(python3 --version) is too old for the test script. Skipping."
    exit
fi

ASAN_OPTIONS=detect_leaks=0 meson test -C "$dir"

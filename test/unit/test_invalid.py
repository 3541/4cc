#!/usr/bin/env python3

import subprocess
import sys
import tempfile

cc = sys.argv[1]
input_path = sys.argv[2]

asm = tempfile.NamedTemporaryFile(suffix = ".asm")
asm_result = subprocess.run([cc, input_path], stdout = subprocess.PIPE)

if asm_result.returncode == 0:
    print(f"FAIL: Expected status 255, but generated code:\n{asm_result.stdout.decode('utf-8')}")
    sys.exit(-1)

if asm_result.returncode != 255:
    print(f"FAIL: Expected status 255, but got {str(asm_result.returncode)}")
    sys.exit(-1)

#!/usr/bin/env python3

import os
import subprocess
import sys
import tempfile

cc = sys.argv[1]
test_path = sys.argv[2]
test_name = sys.argv[3]

input_path = os.sep.join([test_path, test_name + '.c'])
if not os.path.exists(input_path):
    input_path = os.sep.join([test_path, test_name + '.txt'])
output_path = os.sep.join([test_path, test_name + '.out'])

asm = tempfile.NamedTemporaryFile(suffix = ".asm")
object = tempfile.NamedTemporaryFile(suffix = ".o")
binary_path = os.path.splitext(object.name)[0]

subprocess.run([cc, open(input_path).read().strip()], stdout = asm, check = True)
subprocess.run(["nasm", "-felf64", "-o", object.name, asm.name], check = True)
subprocess.run(["gcc", "-o", binary_path, object.name], check = True)

asm.close()
object.close()

exit = subprocess.run([binary_path]).returncode
os.remove(binary_path)

output = open(output_path).read().strip()
if str(exit) != output:
    print(f"FAIL: Expected status {output}, but got {exit}.")
    sys.exit(-1)

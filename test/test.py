#!/usr/bin/env python3

import os
import subprocess
import sys
import tempfile

cc = sys.argv[1]
test_lib = sys.argv[2]
test_path = sys.argv[3]
test_name = sys.argv[4]

input_path = os.sep.join([test_path, test_name + '.c'])
output_path = os.sep.join([test_path, test_name + '.out'])

asm = tempfile.NamedTemporaryFile(suffix = ".asm")
object = tempfile.NamedTemporaryFile(suffix = ".o")
binary_path = os.path.splitext(object.name)[0]

asm_result = subprocess.check_output([cc, input_path])
print(f"Output:\n{asm_result.decode('utf-8')}", file = sys.stderr)
asm.write(asm_result)
asm.flush()

subprocess.run(["nasm", "-felf64", "-o", object.name, asm.name], check = True)
subprocess.run(["gcc", "-o", binary_path, object.name, test_lib], check = True)

asm.close()
object.close()

exit = subprocess.run([binary_path]).returncode
os.remove(binary_path)

output = open(output_path).read().strip()
if str(exit) != output:
    print(f"FAIL: Expected status {output}, but got {exit}.")
    sys.exit(-1)

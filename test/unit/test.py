#!/usr/bin/env python3

import os
import subprocess
import sys
import tempfile

cc = sys.argv[1]
test_lib = sys.argv[2]
input_path = sys.argv[3]
output_path = sys.argv[4]

object = tempfile.NamedTemporaryFile(suffix = ".o")
binary_path = os.path.splitext(object.name)[0]

obj_result = subprocess.check_output([
    cc,
    "-C",
    "-I",
    os.path.join(os.path.dirname(sys.argv[0]), "lib"),
    "-c", input_path,
    "-o", object.name,
    "--dump-ast"
])
print(f"Output:\n{obj_result.decode('utf-8')}", file = sys.stderr)

subprocess.run(["gcc", "-static", "-o", binary_path, object.name, test_lib], check = True)

object.close()

result = subprocess.run([binary_path], stdout = subprocess.PIPE)
exit = result.returncode
os.remove(binary_path)

if os.path.isfile(output_path):
    if output_path.endswith(".out"):
        output = open(output_path).read()
    else:
        p = subprocess.run([output_path])
        output = str(p.returncode)
else:
    output = "0"

if output.strip().isdigit():
    expected_status = output.strip()
else:
    expected_status = "0"

if str(exit) != expected_status:
    print(f"FAIL: Expected status {output.strip()}, but got {exit}.")
    sys.exit(-1)

stdout = result.stdout.decode("utf-8")
if not output.strip().isdigit() and stdout != output:
    print(f"FAIL: Expected output \"{output}\", but got \"{stdout}\".")
    sys.exit(-1)

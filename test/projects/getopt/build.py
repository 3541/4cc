import os
import subprocess
import sys

url = "https://github.com/skeeto/getopt.git"
rev = "55d8fefe680d9b7e68ab80eb46e1bd4ad324fc29"

dir = os.path.dirname(sys.argv[0])
cc = os.path.realpath(sys.argv[1])

work = os.path.join(dir, "work")

if not os.path.exists(work):
    subprocess.run(["git", "clone", url, work], check = True)

subprocess.run(["git", "-C", work, "reset", "--hard", rev], check = True)
subprocess.run(["make", "-C", work, "clean"], check = True)
subprocess.run(["make", "-C", work, "CC=" + cc, "CFLAGS="], check = True)
subprocess.run(["make", "-C", work, "check"], check = True)

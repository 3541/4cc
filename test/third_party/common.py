import os
import subprocess
import sys

def git(dir, *args):
    subprocess.run(["git", "-C", dir] + list(args), check = True)

def make(dir, cc, *args):
    subprocess.run(["make", "-C", dir, "CC=" + cc] + list(filter(lambda a: a, args)), check = True)

def build_and_test(url, rev, cflags = None, ldflags = None, test_target = "test", subdir = None):
    dir = os.path.dirname(sys.argv[0])
    cc = os.path.realpath(sys.argv[1])

    work = os.path.join(dir, "work")
    if not os.path.exists(work):
        subprocess.run(["git", "clone", url, work], check = True)

    git(work, "reset", "--hard", rev)

    if subdir:
        work = os.path.join(work, subdir)

    make(work, cc, "clean")
    make(work, cc, "CFLAGS=" + cflags if cflags else None, "LDFLAGS=" + ldflags if ldflags else None)
    make(work, cc, test_target)

import os
import subprocess

def git(dir, *args):
    subprocess.run(["git", "-C", dir] + list(args), check = True)

def make(dir, cc, *args):
    subprocess.run(["make", "-C", dir, "CC=" + cc] + list(filter(lambda a: a, args)), check = True)

def build_and_test(dir, cc, url, rev, cflags = None, test_target = "test"):
    work = os.path.join(dir, "work")
    if not os.path.exists(work):
        subprocess.run(["git", "clone", url, work], check = True)

    git(work, "reset", "--hard", rev)
    make(work, cc, "clean")
    make(work, cc, "CFLAGS=" + cflags if cflags else None)
    make(work, cc, test_target)

import os
import subprocess

def git(dir, *args):
    subprocess.run(["git", "-C", dir] + list(args), check = True)

def make(dir, cc, *args):
    subprocess.run(["make", "-C", dir, "CC=" + cc] + list(args), check = True)

def build_and_test(dir, cc, url, rev):
    work = os.path.join(dir, "work")
    if not os.path.exists(work):
        subprocess.run(["git", "clone", url, work], check = True)

    git(work, "reset", "--hard", rev)
    make(work, cc, "clean")
    make(work, cc)
    make(work, cc, "test")

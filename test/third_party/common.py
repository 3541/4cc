from enum import Enum, auto
from pathlib import Path
from typing import Optional
import os
import subprocess
import sys


class Flavor(Enum):
    AUTOTOOLS = auto()
    MAKEFILE = auto()


def git(dir: Path, *args) -> None:
    subprocess.run(["git", "-C", dir] + list(args), check=True)


def make(dir: Path, cc: Path, *args) -> None:
    subprocess.run(["make", "-C", dir, "CC=" + cc] + list(filter(lambda a: a, args)), check=True)


def configure(dir: Path, cc: Path) -> None:
    subprocess.run(["./configure"], env=os.environ | {"CC": str(cc)}, cwd=dir, check=True)


def build_and_test(
    url: str,
    rev: str,
    cflags: Optional[str] = None,
    ldflags: Optional[str] = None,
    test_target: str = "test",
    subdir: Optional[Path] = None,
    flavor: Flavor = Flavor.MAKEFILE,
) -> None:
    dir = os.path.dirname(sys.argv[0])
    cc = os.path.realpath(sys.argv[1])

    work = os.path.join(dir, "work")
    if not os.path.exists(work):
        subprocess.run(["git", "clone", url, work], check=True)

    git(work, "reset", "--hard", rev)

    if subdir:
        work = os.path.join(work, subdir)

    if flavor == Flavor.AUTOTOOLS:
        configure(work, cc)

    make(work, cc, "clean")
    make(
        work, cc, "CFLAGS=" + cflags if cflags else None, "LDFLAGS=" + ldflags if ldflags else None
    )
    make(work, cc, test_target)

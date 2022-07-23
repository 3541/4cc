#!/usr/bin/env python3

import sys
import os

valid = sys.argv[1]
dir = sys.argv[2]

names = set()

for root, _, files in os.walk(dir):
    if "stage" not in root:
        continue
    if valid == "valid" and "invalid" in root:
        continue
    if valid == "invalid" and "invalid" not in root:
        continue
    for file in files:
        name = os.path.splitext(file)[0]
        while name in names:
            name = name + "_"
        names.add(name)

        print(name + "\0" + os.path.join(root, file))

#!/usr/bin/env python3

import sys
import os

valid = sys.argv[1]
dir = sys.argv[2]
stage = sys.argv[3]

dir = os.path.join(dir, 'stage_' + stage, valid)
for root, _, files in os.walk(dir):
    for file in files:
        print(os.path.join(root, file))

#!/usr/bin/env python3

import sys
import os

valid = sys.argv[1]
dir = sys.argv[2]
stage = sys.argv[3]

dir = os.path.join(dir, 'stage_' + stage, valid)
for file in os.listdir(dir):
    print(os.path.join(dir, file))

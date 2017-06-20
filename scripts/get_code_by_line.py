#!/usr/bin/env python2
import sys
from base64 import b64decode as b

lineno = sys.argv[1]

with open('output.csv') as f:
    lines = f.readlines()

print(b(lines[int(lineno)].split(',')[2].strip()))

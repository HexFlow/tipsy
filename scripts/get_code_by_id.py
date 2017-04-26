#!/usr/bin/env python2
import sys
from base64 import b64decode as b

idd = sys.argv[1]

with open('output.csv') as f:
    lines = f.readlines()

for line in lines:
    if line.split(',')[0].strip() == idd:
        print(b(line.split(',')[2].strip()))

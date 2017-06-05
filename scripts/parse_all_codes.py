#!/usr/bin/env python2
from base64 import b64decode as b
import os

with open('output.csv', 'r') as f:
    codes = [b(k.split(',')[2].strip()) for k in f.readlines()]

if not os.path.exists('parsedCodes'):
    os.mkdir('parsedCodes')

for i in range(len(codes)):
    with open('parsedCodes/code' + str(i) + '.c', 'w') as f:
        f.write(codes[i])

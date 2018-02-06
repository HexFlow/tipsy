#!/usr/bin/env python2
from base64 import b64decode as b
import os

with open('output.csv', 'r') as f:
    contents = f.readlines()
codes = [b(k.split(',')[2].strip()) for k in contents]
# scores = [int(k.split(',')[3].strip()) for k in contents]

if not os.path.exists('Lab11Q2'):
    os.mkdir('Lab11Q2')

for i in range(len(codes)):
    with open('Lab11Q2/code' + str(i) + '.c', 'w') as f:
        f.write(codes[i])
    # with open('Lab11Q2/score' + str(i), 'w') as f:
    #     f.write(str(scores[i]))

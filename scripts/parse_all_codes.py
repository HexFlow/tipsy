#!/usr/bin/env python2
from base64 import b64decode as b
import os
import sys

assert(len(sys.argv) >= 3)
labNo = sys.argv[1]
probId = sys.argv[2]

folderName = 'Codes-{}-{}'.format(labNo, probId)

with open('output-{}-{}.csv'.format(labNo, probId)) as f:
    contents = f.readlines()
codes = [b(k.split(',')[2].strip()) for k in contents]
# scores = [int(k.split(',')[3].strip()) for k in contents]

if not os.path.exists(folderName):
    os.mkdir(folderName)

for i in range(len(codes)):
    with open(folderName+'/code' + str(i) + '.c', 'w') as f:
        f.write(codes[i])
    # with open('Lab11Q2/score' + str(i), 'w') as f:
    #     f.write(str(scores[i]))

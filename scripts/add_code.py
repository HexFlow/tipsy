#!/usr/bin/env python2
import requests as r
from base64 import b64decode as b
import json
import sys

assert(len(sys.argv) >= 3)
labNo = sys.argv[1]
probId = sys.argv[2]

with open('output-{}-{}.csv'.format(labNo, probId)) as f:
    lines = [ k.split(',') for k in f.readlines() ]

cnt = 0
for line in lines:
    body = {
            "userId": line[1].strip(),
            "quesId": '{}-{}'.format(labNo, probId),
            "code": b(line[2].strip()).decode("utf-8"),
            "score": line[3].strip(),
            "updateClusters": True,
            "file": "Codes-{}-{}/code{}.c".format(labNo, probId, str(cnt))
            }
    headers = {'Content-type': 'application/json', 'Accept': 'application/json'}
    req = r.post('http://localhost:8070/api/submit', data=json.dumps(body), headers=headers)
    print(line[0].strip())
    print(req.text, req.status_code)
    print("Finished processing code" + str(cnt) + ".c")
    cnt+=1

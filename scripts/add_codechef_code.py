#!/usr/bin/env python2
import requests as r
from base64 import b64decode as b
import json
import sys
import os

assert(len(sys.argv) >= 3)
quesId = sys.argv[1]

cnt = 0
for line in os.listdir(sys.argv[2]):
    if cnt > 1000:
        break

    with open(sys.argv[2] + "/" + line) as f:
        body = {
                "userId": "user-{}".format(str(cnt)),
                "quesId": quesId,
                "code": f.read(),
                "score": "20",
                # "updateClusters": True,
                "file": line
                }
        headers = {'Content-type': 'application/json', 'Accept': 'application/json'}
        req = r.post('http://localhost:8070/api/submit', data=json.dumps(body), headers=headers)
        print(line[0].strip())
        print(req.text, req.status_code)
        print("Finished processing code" + str(cnt) + ".c")
        cnt+=1

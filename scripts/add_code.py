#!/usr/bin/env python2
import requests as r
from base64 import b64decode as b
import json

with open('output.csv') as f:
    lines = [ k.split(',') for k in f.readlines() ]

for line in lines:
    body = {
            "userId": line[0].strip(),
            "quesId": "401",
            "code": b(line[2].strip()).decode("utf-8")
            }
    headers = {'Content-type': 'application/json', 'Accept': 'application/json'}
    req = r.post('http://localhost:8070/api/submit', data=json.dumps(body), headers=headers)
    print(line[0].strip())
    print(req.text, req.status_code)

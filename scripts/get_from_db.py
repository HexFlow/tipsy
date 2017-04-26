#!/usr/bin/env python2
from base64 import b64decode
import pymysql.cursors
import pymysql

connection = pymysql.connect(host='localhost', user='root', db='esc101')
labNo = 'LAB-4'
queNo = 1

try:
    with connection.cursor() as cursor:
        sql = """
        SELECT code_id, user_id, contents FROM esc101.code as C JOIN
          (SELECT code_id, verdict FROM esc101.evaluation
           WHERE assignment_id IN
             (SELECT id FROM esc101.assignment
              WHERE event_name = "{0}"
              AND question = {1})
           GROUP BY code_id
           HAVING count(DISTINCT verdict)=1 AND verdict="ACCEPTED") as A
        ON A.code_id = C.id;
        """.format(labNo, queNo)
        cursor.execute(sql)
        result = cursor.fetchall()
        with open('output.csv', 'w') as f:
            for r in result:
                f.write(','.join([str(a) for a in r]))
                f.write('\n')
except Exception as e:
    print(str(e))
finally:
    connection.close()

#!/usr/bin/env python2
from base64 import b64decode
import pymysql.cursors
import pymysql

connection = pymysql.connect(host='localhost', user='root', db='prutor3', password='csssucks')
labNo = 'Lab-11'
queNo = 2

try:
    with connection.cursor() as cursor:
        sql2 = """
        SELECT code_id, user_id, contents FROM prutor3.code as C JOIN
          (SELECT code_id, verdict FROM prutor3.evaluation as E JOIN
            (SELECT id, score FROM prutor3.assignment as A)
           WHERE assignment_id IN
             (SELECT id FROM prutor3.assignment
              WHERE event_name = "{0}"
              AND question = {1})
           GROUP BY code_id
           HAVING count(distinct verdict)=1 AND verdict="ACCEPTED") as A
        ON A.code_id = C.id;
       """.format(labNo, queNo)
        sql1 = """
            select c.id, c.user_id, c.contents, a.score, e.verdict
            from code c, evaluation e, assignment a
            where ((c.id = e.code_id) and (e.assignment_id = a.id) and
                   (a.event_name = "{0}" and a.question = "{1}")   and
                   (a.score is not NULL));
        """.format(labNo, queNo)
        sql = """
             ( select c.id, c.user_id, c.contents, a.score
            from code c, assignment a, account ac
            where ((c.id = a.submission) and
                   (a.event_name = "{0}" and a.question = {1}) and
                   (a.score is not NULL) and
                   (a.user_id=ac.id) and
                   (ac.section rlike 'B[0-9]+'))
            group by c.id );
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

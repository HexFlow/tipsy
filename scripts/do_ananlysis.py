#!/usr/bin/env python2
import json
import sys
import os

DIR=sys.argv[2]
LINK=sys.argv[3]
os.system("""grep -A2 "Name" {0}/result | tail -1 | sed -E 's/List//g' | sed -E 's/\(/\[/g' | sed -E 's/\)/\]/g' | sed -E 's/(s[^,]*\.c)/"\\1"/g' > {0}/names""".format(DIR))
os.system("""grep -A2 "Matrix" {0}/result | tail -1 | sed -E 's/List//g' | sed -E 's/\(/\[/g' | sed -E 's/\)/\]/g' > {0}/matrix""".format(DIR))
os.system("""grep -A2 "Cluster.*K-Mean" {0}/result | tail -1 | sed -E 's/List\(//g' | sed -E 's/(.*)\)/\\1/g' > {0}/cluster""".format(DIR))

total = int(sys.argv[1])

with open('{0}/names'.format(DIR)) as f:
    n = json.load(f)
with open('{0}/cluster'.format(DIR)) as f:
    c = f.readlines()[0].strip()
with open('{0}/matrix'.format(DIR)) as f:
    mat = json.load(f)

c = c.split('), (')
c[0] = c[0][1:]
c[len(c) - 1] = c[len(c) - 1][:-1]

for i in range(len(c)):
   c[i] = int(c[i].split(',')[1])

cluster = []
for i in range(total):
    cluster.append([])

for i in range(len(c)):
    cluster[c[i]].append(i)

scores = []
for i in range(total):
    scores.append([])
def getScore(ind):
    code = n[ind].split('/')[2][4:-2]
    sf = '../../scripts/{0}/score'.format(DIR.split('/')[0]) + code
    with open(sf) as f:
        s = int(f.readlines()[0])
    return s

for i in range(1, total+1):
    for j in range(len(cluster[i-1])):
        s = getScore(cluster[i-1][j])
        scores[i-1].append(s)

names = [[]]
for i in range(1,total+1):
    names.append([])
    for j in range(len(cluster[i-1])):
        names[i].append(n[cluster[i-1][j]])

def mean(a):
    return sum(a)/float(len(a))

def stdDev(a):
    mn = mean(a)
    var = 0.0
    for i in range(len(a)):
        var += (a[i] - mn)**2
    var /= len(a)
    import math as mth
    return mth.sqrt(var)

analysis = [()]
for i in range(1, total+1):
    analysis.append((min(scores[i - 1]), max(scores[i - 1]), stdDev(scores[i - 1])))

def indexOf(a, b):
    for i in range(len(a)):
        if a[i]==b:
            return i
    return -1

def wrap(s):
    return ("""<a href="{0}&preview={1}">{1}</a>""".format(LINK,s))


for i in range(1, total+1):
        print "-----------<br>"
        print "Cluster {0}<br>".format(i)
        print "-----------<br>"
        for j in range(len(cluster[i-1])):
            print str(indexOf(n, names[i][j])) + '-' + wrap(names[i][j].split('/')[2]) + '&nbsp;'*5 + str(scores[i-1][j]) + '<br>'
        print str(analysis[i]) + '<br>'
        print "------------------<br>"
        print "PairWise Distance:<br>"
        print "------------------<br>"
        print "-----------<br>"
        print "Cluster {0}<br>".format(i)
        print "-----------<br>"
        for j in range(len(cluster[i-1])):
            for k in range(len(cluster[i-1])):
                if k <= j:
                    continue
                print wrap(names[i][j].split('/')[2]) + '&nbsp;'*5 + wrap(names[i][k].split('/')[2]) + '&nbsp;'*5 + str(mat[cluster[i-1][j]][cluster[i-1][k]]) + '<br>'
        print "<br><br><br><br>"

#import matplotlib.pyplot as plt
#
#for i in scores[1:]:
#    plt.hist(i)
#    plt.show()

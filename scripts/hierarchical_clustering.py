#!/usr/bin/env python2

from __future__ import print_function

import PyQt4
import matplotlib

from matplotlib import pyplot as plt
from scipy.cluster.hierarchy import dendrogram, linkage, cophenet, to_tree
import numpy as np
import json
import sys
import os

SHOWPLOT = 0
if len(sys.argv) >= 2 and sys.argv[1] == "showplot":
    SHOWPLOT = 1

forId = {}
idMap = {}
idRevMap = {}
cnt = 0

def extract(d):
    a = d.strip()[1:-1].split(',')
    return (int(a[0].strip()), float(a[1].strip()))

for line in sys.stdin.readlines():
    sp = line.split(':')
    id = int(sp[0].strip())
    res = sp[1].strip().split('|')
    matches = map(extract, res)
    forId[id] = dict(matches)

for key in forId.keys():
    idRevMap[cnt] = key
    idMap[key] = cnt
    cnt += 1

matrixNetwork = np.zeros(shape=(cnt, cnt))
for i in range(0, cnt):
    for j in range(0, cnt):
        if i == j:
            matrixNetwork[i][j] = 0
        else:
            matrixNetwork[i][j] = forId[idRevMap[i]][idRevMap[j]]

print(matrixNetwork, file = sys.stderr)

compressedMatrixNetwork = matrixNetwork[np.triu_indices(len(matrixNetwork), 1)]

# hcMethods = ['single', 'complete', 'average', 'weighted', 'centroid', 'median', 'ward']
# centroid, median, ward will only work if euclidean distance is used that is an embedding of distances between parsetrees is possible in k-dim vector space with l2 norm
hcMethods = ['single', 'complete', 'average', 'weighted']
mx = 0.0
method = 'single'
for method_ in hcMethods:
    linked = linkage(compressedMatrixNetwork, method_)
    coph_var, _ = cophenet(linked, compressedMatrixNetwork)
    if mx < coph_var:
        mx = coph_var
        method = method_

if method in ['centroid', 'median', 'ward']:
    print('** [warning] ' + method + ' method will work only when euclidean distance exists for set of points', file = sys.stderr)

print(method, mx, file = sys.stderr)

def fancy_dendrogram(*args, **kwargs):
    max_d = kwargs.pop('max_d', None)
    if max_d and 'color_threshold' not in kwargs:
        kwargs['color_threshold'] = max_d
    annotate_above = kwargs.pop('annotate_above', 0)

    ddata = dendrogram(*args, **kwargs)

    if not kwargs.get('no_plot', False):
        plt.title('Dendrogram for matrixNetwork')
        plt.xlabel('cluster size')
        plt.ylabel('distance')
        for i, d, c in zip(ddata['icoord'], ddata['dcoord'], ddata['color_list']):
            x = 0.5 * sum(i[1:3])
            y = d[1]
            if y > annotate_above:
                plt.plot(x, y, 'o', c = c)
                plt.annotate("%.3g" % y, (x, y), xytext = (0, -5),
                             textcoords = 'offset points',
                             va = 'top', ha='center')

        if max_d:
            plt.axhline(y = max_d, c = 'k')
    return ddata

linked = linkage(compressedMatrixNetwork, method)
#plt.figure(figsize=(25,10))
#plt.title('Dendrogram for Matrix')
#plt.xlabel('codes')
#plt.ylabel('distance')
dend = fancy_dendrogram(linked,
                        leaf_rotation = 90,
                        leaf_font_size = 8,
                      # truncate_mode = 'lastp',
                      # p = 12,
                        show_contracted = True,
                        annotate_above = 1000,
                        max_d = 600)

if SHOWPLOT == 1:
    plt.show()

hierarchicalTree = to_tree(linked)

clusters = [(i, -1) for i in range(0, len(matrixNetwork))]
outliers = []
clusterCount = 0
thresholdDist = 700.0
thresholdCount = (4, 15) # (min, max)

def assign(rootnode):
    if rootnode is None:
        return
    elif rootnode.count == 1:
        clusters[rootnode.id] = (rootnode.id, clusterCount)
    else:
        assign(rootnode.left)
        assign(rootnode.right)

def markAsOutlier(rootnode):
    if rootnode is None:
        return
    elif rootnode.count == 1:
        outliers.append(rootnode.id)
    else:
        markAsOutlier(rootnode.left)
        markAsOutlier(rootnode.right)

def dfs(rootnode, parentnode = None):
    global clusterCount
    if rootnode is None:
        return
    elif rootnode.count >= 1 and rootnode.count <= 3:
        if parentnode is not None and parentnode.dist >= thresholdDist:
            markAsOutlier(rootnode)
        else:
            assign(rootnode)
            clusterCount += 1
    elif rootnode.count >= thresholdCount[0] and rootnode.count <= thresholdCount[1]:
        assign(rootnode)
        clusterCount += 1
        if parentnode is not None and parentnode.dist >= thresholdDist:
            print("[warning] a cluster was made before thresholdDist", file = sys.stderr)
    else:
        dfs(rootnode.left, rootnode)
        dfs(rootnode.right, rootnode)

dfs(hierarchicalTree)

print(clusterCount, file = sys.stderr)
print(thresholdDist, thresholdCount, file = sys.stderr)
print(clusters, file = sys.stderr)
print(outliers, file = sys.stderr)

for i in outliers:
    clusters[i] = (i, clusterCount)
if len(outliers) > 0:
    clusterCount += 1

def mkFinal(a):
    id, cnt = a
    return (idRevMap[id], cnt)

finalclusters = [[] for i in range(clusterCount)]
for cluster in clusters:
    finalclusters[cluster[1]].append(idRevMap[cluster[0]])
print(finalclusters, file = sys.stderr)

def rmse(i, lis):
    ret = 0.0
    for j in range(len(lis)):
        ret += matrixNetwork[idMap[i]][idMap[lis[j]]] ** 2
    return (ret / 2.0) ** 0.5

def reorder(lis):
    a = []
    for i in range(len(lis)):
        a.append((rmse(lis[i], lis), lis[i]))
    a.sort()
    return [i[1] for i in a]

for i in range(clusterCount):
    print(','.join(map(str, reorder(finalclusters[i]))))

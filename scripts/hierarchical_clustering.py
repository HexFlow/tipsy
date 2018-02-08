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

matplotlib.rcParams.update({'font.size': 18})

SHOWPLOT = 0
if len(sys.argv) >= 2 and sys.argv[1] == "showplot":
    SHOWPLOT = 1

idMap = {}
idRevMap = {}
cnt = 0

di = {}
inp = sys.stdin.read()
j = json.loads(inp)
for entry in j:
    if entry[0] not in di:
        di[entry[0]] = {}
    if entry[1] not in di:
        di[entry[1]] = {}
    di[entry[0]][entry[1]] = entry[2]
    di[entry[1]][entry[0]] = entry[2]
    if entry[0] not in idMap:
        idMap[entry[0]] = cnt
        idRevMap[cnt] = entry[0]
        cnt += 1
    if entry[1] not in idMap:
        idMap[entry[1]] = cnt
        idRevMap[cnt] = entry[1]
        cnt += 1

matrixNetwork = np.zeros(shape=(cnt, cnt))
for i in range(cnt):
    for j in range(cnt):
        if i is not j:
            matrixNetwork[i][j] = di[idRevMap[i]][idRevMap[j]]

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

linked = linkage(compressedMatrixNetwork, method)

def fancy_dendrogram(*args, **kwargs):
    max_d = kwargs.pop('max_d', None)
    if max_d and 'color_threshold' not in kwargs:
        kwargs['color_threshold'] = max_d
    annotate_above = kwargs.pop('annotate_above', 0)

    ddata = dendrogram(*args, **kwargs)

    if not kwargs.get('no_plot', False):
        plt.title('Dendrogram: Clusters before running PruneTree')
        plt.xlabel('Program ID')
        plt.ylabel('Distance')
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

if SHOWPLOT == 1:
    plt.figure(figsize=(25,10))
    plt.title('Dendrogram: Clusters before running PruneTree')
    # plt.xlabel('Program ID')
    plt.ylabel('Distance')
    dend = fancy_dendrogram(linked,
                            leaf_rotation = 90,
                            leaf_font_size = 8,
                        # truncate_mode = 'lastp',
                        # p = 12,
                            show_contracted = True,
                            annotate_above = 400,
                            max_d = 300)
    plt.show()

hierarchicalTree = to_tree(linked)

clusters = [(i, -1) for i in range(0, len(matrixNetwork))]
clusterCount = 0
thresholdDist = 400.0
thresholdCount = int(cnt ** 0.5) # (min, max)

def assign(rootnode):
    if rootnode is None:
        return
    elif rootnode.count == 1:
        clusters[rootnode.id] = (rootnode.id, clusterCount)
    else:
        assign(rootnode.left)
        assign(rootnode.right)

def dfs(rootnode = None):
    global clusterCount
    if rootnode is None:
        return
    elif rootnode.dist > thresholdDist or rootnode.count >= 2*thresholdCount:
        dfs(rootnode.left)
        dfs(rootnode.right)
    elif rootnode.count <= thresholdCount:
        assign(rootnode)
        clusterCount += 1
    elif rootnode.left is None:
        dfs(rootnode.right)
    elif rootnode.right is None:
        dfs(rootnode.left)
    elif rootnode.left.count <= thresholdCount:
        assign(rootnode.left)
        clusterCount += 1
        dfs(rootnode.right)
    else:
        assign(rootnode.right)
        clusterCount += 1
        dfs(rootnode.left)

dfs(hierarchicalTree)

print(clusterCount, file = sys.stderr)
print(thresholdDist, thresholdCount, file = sys.stderr)
print(clusters, file = sys.stderr)

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

print(json.dumps(map(reorder, finalclusters)))
# res = map(lambda x: ','.join(map(str, reorder(x))), finalclusters)
# print('|'.join(res))

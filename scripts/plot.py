import matplotlib.pyplot as plt
import sys

def chunks(l, n):
    print (len(l))
    for i in range(0, len(l)-n):
        yield l[i:i + n]

with open(sys.argv[1]) as f:
    _lines = [int(k) for k in f.readlines()]
    lines = []
    for arr in chunks(_lines, 20):
        lines.append(float(sum(arr))/len(arr)/1000)

fig, ax = plt.subplots(1, 1)
out = ax.plot(lines)
# plt.xlabel("Number of programs in database")

# plt.ylabel("Time to form clusters (s)")
# plt.ylabel("Time to recompute distances on program addition (s)")

fig.savefig(sys.argv[1] + ".png")
plt.show()


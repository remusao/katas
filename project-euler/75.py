
from collections import Counter
import numpy as np


def gen_prim_pyth_trips(limit=None):
    u = np.mat(' 1  2  2; -2 -1 -2; 2 2 3')
    a = np.mat(' 1  2  2;  2  1  2; 2 2 3')
    d = np.mat('-1 -2 -2;  2  1  2; 2 2 3')
    uad = np.array([u, a, d])
    m = np.array([3, 4, 5])
    while m.size:
        m = m.reshape(-1, 3)
        if limit:
            m = m[m[:, 2] <= limit]
        for e in m:
            yield e
        m = np.dot(m, uad)


def gen_all_pyth_trips(limit):
    for prim in gen_prim_pyth_trips(limit):
        i = prim
        for _ in range(limit//prim[2]):
            yield i
            i = i + prim

def main():
    count = Counter()
    limit = 1500000
    N = int(10e4)
    for triplet in gen_all_pyth_trips(N):
        s = sum(triplet)
        if s <= limit:
            count[s] += 1
    print len([v for v in count.values() if v == 1])
main()

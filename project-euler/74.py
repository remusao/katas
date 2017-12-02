
from __future__ import print_function
import math
import itertools


def step(n):
    return sum(math.factorial(int(i)) for i in str(n))

def main():
    N = 1000000
    memo = {
        0: 1,
        1: 1,
        2: 1,
        145: 1,
        169: 3,
        871: 2,
        872: 2,
        1454: 3,
        363601: 3,
        40585: 1,
        45361: 2,
        45362: 2,
    }

    for i in xrange(N):
        size = 0
        tmp = i
        cycle = memo.get(tmp)
        chain = [tmp]
        while cycle is None:
            tmp = step(tmp)
            chain.append(tmp)
            cycle = memo.get(tmp)
            size += 1
        size += cycle
        for e in chain:
            memo[e] = size
            size -= 1
        # print(i, size)

    res = 0
    for i, s in memo.iteritems():
        if s == 60:
            res += 1
            print("Good:", i, s)
    print(res)

if __name__ == "__main__":
    main()

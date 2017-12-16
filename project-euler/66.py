
from decimal import *
from itertools import islice
import math

getcontext().prec = 100

def continued(x):
    n = Decimal(x).sqrt()
    while True:
        i = int(n)
        yield i
        n = 1 / (n - i)

def convergents(x):
    hn1 = 0
    hn2 = 1

    kn1 = 1
    kn2 = 0

    for n in continued(x):
        hn = n * hn2 + hn1
        hn1 = hn2
        hn2 = hn
        kn = n * kn2 + kn1
        kn1 = kn2
        kn2 = kn
        yield hn2, kn2

def is_square(n):
    r = int(math.sqrt(n))
    return r * r == n

def solve(x, y, d):
    return x * x - d * y * y == 1


def main():
    max_x = 0
    max_d = 0
    for d in range(2, 1001):
        if is_square(d):
            continue

        print('D =', d)
        for (h, k) in convergents(d):
            if solve(h, k, d):
                if h > max_x:
                    max_x = h
                    max_d = d
                print('SOLVE', h, k)
                break
    print(max_d, max_x)

if __name__ == '__main__':
    main()

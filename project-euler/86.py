

from __future__ import print_function
from math import sqrt
from collections import Counter
from pprint import pprint


def main():
    count = 0
    a = 0
    while True:
        a += 1
        for b in range(1, a + 1):
            for c in range(1, b + 1):
                shortest = sqrt(a * a + (b + c) * (b + c))
                if abs(shortest - int(shortest)) < 1e-6:
                    print("a: %s, b: %s, c: %s :: shortest: %s" % (a, b, c, shortest))
                    count += 1
                    if count >= 100:
                        print(a)
                        return
    print(count)


if __name__ == "__main__":
    main()

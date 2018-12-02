

import sys
from collections import Counter


def solve1():
    threes = 0
    twos = 0
    for line in sys.stdin:
        counts = frozenset(Counter(line).values())
        if 3 in counts:
            threes += 1
        if 2 in counts:
            twos += 1
    print(threes * twos)


if __name__ == "__main__":
    solve1()

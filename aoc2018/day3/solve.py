#! /usr/bin/env python

import re
import sys
import random
import itertools
from collections import Counter


def iter_grids():
    """A grid is a set of 2D coordinates. """
    for line in sys.stdin:
        cid, x, y, w, h = map(int, re.findall(r'\d+', line))
        yield cid, frozenset({
            i + j * 1j
            for i in range(x, x + w)
            for j in range(y, y + h)
        })


def solve1():
    union = set()
    inter = set()
    for _, grid in iter_grids():
        inter |= union & grid
        union |= grid
    print(f'{len(inter)} square inches of fabric overlap')


def solve2():
    grids = list(iter_grids())
    coords = Counter(itertools.chain.from_iterable(grid for _, grid in grids))
    for cid, grid in grids:
        if len(grid) == sum(coords[coord] for coord in grid):
            print(f'Claim {cid} does not overlap')
            break

def main():
    # solve1()
    solve2()

if __name__ == '__main__':
    main()

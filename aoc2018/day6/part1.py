#!/usr/bin/env python
# -*- coding: utf-8 -*-

from collections import Counter
import itertools
import sys


def find_closest(coordinates, x, y):
    distances = sorted(
        (abs(coord.real - x) + abs(coord.imag - y), i)
        for i, coord in enumerate(coordinates)
    )

    if distances[0][0] == distances[1][0]:
        return "."

    return distances[0][1]


def main():
    coordinates = []
    max_x, max_y = 0, 0
    for line in sys.stdin:
        x, y = map(int, line.strip().split(", "))
        coordinates.append(x + y * 1j)
        max_x = max(max_x, x)
        max_y = max(max_y, y)

    # Find points with infinite areas
    grid = {
        x + y * 1j: find_closest(coordinates, x, y)
        for y in range(-1, max_y + 2)
        for x in range(-1, max_x + 2)
    }

    # Identify starting points with infinite areas
    infinites = frozenset(
        grid[coord]
        for coord in itertools.chain(
            (-1j + x for x in range(0, max_x + 1)),
            ((max_y + 1) * 1j + x for x in range(0, max_x + 1)),
            (-1 + y * 1j for y in range(0, max_y + 1)),
            (max_x + 1 + y * 1j for y in range(0, max_y + 1)),
        )
    )

    print(
        "Part 1",
        Counter(v for v in grid.values() if v not in infinites).most_common(1)[0][1],
    )


if __name__ == "__main__":
    main()

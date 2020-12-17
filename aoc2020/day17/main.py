#!/usr/bin/env python


import time
import functools
import collections
import itertools


DELTAS = [
    [
        tuple(delta)
        for delta in itertools.product(*([(-1, 0, 1)] * n))
        if not all(d == 0 for d in delta)
    ]
    for n in range(0, 6)
]


@functools.cache
def neighbors(coordinates):
    n = len(coordinates)
    return [tuple(coordinates[i] + delta[i] for i in range(n)) for delta in DELTAS[n]]


def parse_grid(lines, n):
    grid = set()
    for y, line in enumerate(lines):
        for x, c in enumerate(line.strip()):
            if c == "#":
                grid.add((x, y) + tuple([0] * (n - 2)))
    return grid


def run(grid):
    counts = collections.Counter()
    for coordinates in grid:
        counts.update(neighbors(coordinates))

    for _ in range(6):
        to_deactivate = {
            coordinates
            for coordinates, c in counts.items()
            if c != 2 and c != 3 and coordinates in grid
        }

        to_activate = {
            coordinates
            for coordinates, c in counts.items()
            if c == 3 and coordinates not in grid
        }

        grid |= to_activate
        counts.update(
            itertools.chain.from_iterable(
                neighbors(coordinates) for coordinates in to_activate
            )
        )

        grid -= to_deactivate
        counts.subtract(
            itertools.chain.from_iterable(
                neighbors(coordinates) for coordinates in to_deactivate
            )
        )

    return len(grid)


def main():
    with open("./input1.txt") as inputs:
        lines = list(inputs)
        grid3 = parse_grid(lines, 3)
        grid4 = parse_grid(lines, 4)

        # Warm-up
        for _ in range(5):
            run(grid3.copy())
            run(grid4.copy())

        # Display results
        print("Part 1:", run(grid3.copy()))
        print("Part 2:", run(grid4.copy()))

        t0 = time.time()
        run(grid3.copy())
        run(grid4.copy())
        t1 = time.time()
        print("Time:", t1 - t0)

        # Check higher dimensions
        for n in range(3, 6):
            t0 = time.time()
            result = run(parse_grid(lines, n))
            t1 = time.time()
            print(f"n={n}, result={result}, total={t1 - t0}")


if __name__ == "__main__":
    main()

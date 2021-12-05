#!/usr/bin/env python

from collections import Counter
import cmath


def solve1():
    with open("./input.txt") as inputs:
        points = Counter()
        for line in inputs:
            start, end = line.strip().split(" -> ")
            x1, y1 = map(int, start.split(","))
            x2, y2 = map(int, end.split(","))
            if x1 == x2:
                points.update(
                    x1 + min(y1, y2) * 1j + delta * 1j
                    for delta in range(0, max(y1, y2) - min(y1, y2) + 1)
                )
            elif y1 == y2:
                points.update(
                    min(x1, x2) + y1 * 1j + delta
                    for delta in range(0, max(x1, x2) - min(x1, x2) + 1)
                )

        return sum(1 for overlap in points.values() if overlap >= 2)


def solve2():
    with open("./input.txt") as inputs:
        points = Counter()
        for line in inputs:
            start, end = line.strip().split(" -> ")
            x1, y1 = map(int, start.split(","))
            x2, y2 = map(int, end.split(","))

            slope = None
            if x1 == x2:
                if y1 > y2:
                    slope = -1j
                else:
                    slope = 1j
            elif y1 == y2:
                if x1 > x2:
                    slope = -1
                else:
                    slope = 1
            elif x1 < x2:
                if y1 < y2:
                    slope = 1 + 1j
                else:
                    slope = 1 - 1j
            elif x1 > x2:
                if y1 < y2:
                    slope = -1 + 1j
                else:
                    slope = -1 - 1j

            p1 = x1 + y1 * 1j
            p2 = x2 + y2 * 1j
            points[p1] += 1
            while p1 != p2:
                p1 += slope
                points[p1] += 1

        return sum(1 for overlap in points.values() if overlap >= 2)


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

    import time

    n = 10
    t0 = time.time()
    for _ in range(n):
        solve1()
        solve2()
    t1 = time.time()
    print("Total:", (t1 - t0) / n)

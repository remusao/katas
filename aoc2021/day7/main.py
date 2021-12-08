#!/usr/bin/env python

import sys


def solve(positions, cost):
    min_pos = min(positions)
    max_pos = max(positions)

    best_score = sys.maxsize
    for target_pos in range(min_pos, max_pos + 1):
        score = sum(cost(abs(target_pos - position)) for position in positions)
        if score < best_score:
            best_score = score

    return best_score


def solve1():
    with open("./input.txt") as inputs:
        return solve(
            [int(pos) for pos in inputs.read().strip().split(",")], cost=lambda n: n
        )


def solve2():
    with open("./input.txt") as inputs:
        return solve(
            [int(pos) for pos in inputs.read().strip().split(",")],
            cost=lambda n: int((n * (n + 1)) / 2),
        )


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

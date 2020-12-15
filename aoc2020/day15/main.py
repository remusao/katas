#!/usr/bin/env python

import array


def run(numbers, stop):
    last_seen = array.array("I", [0] * stop)
    for i, n in enumerate(numbers[:-1]):
        last_seen[n] = i + 1

    i = len(numbers)
    n0 = numbers[-1]
    l, last_seen[n0] = last_seen[n0], i

    while i != stop:
        n0 = 0 if l == 0 else i - l
        i += 1
        l, last_seen[n0] = last_seen[n0], i

    return n0


def solve1(numbers):
    return run(numbers, 2020)


def solve2(numbers):
    return run(numbers, 30000000)


if __name__ == "__main__":
    import time

    inputs = "0,3,6"
    inputs = "9,12,1,4,17,0,18"
    numbers = list(map(int, inputs.split(",")))

    print("Part 1:", solve1(numbers))
    print("Part 2:", solve2(numbers))

    t0 = time.time()
    solve2(numbers)
    t1 = time.time()
    print(t1 - t0)

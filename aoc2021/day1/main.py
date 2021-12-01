#!/usr/bin/env python


def read():
    with open("./input.txt") as inputs:
        for line in inputs:
            yield int(line)


def sliding_windows(size=1):
    values = list(read())
    for i in range(0, len(values) - size + 1):
        yield values[i : i + size]


def solve(window_size):
    solution = 0
    prev = None
    for window in sliding_windows(window_size):
        new = sum(window)
        if prev is not None and new > prev:
            solution += 1
        prev = new
    return solution


def solve1():
    return solve(1)


def solve2():
    return solve(3)


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

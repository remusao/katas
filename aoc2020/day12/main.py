#!/usr/bin/env python

import time


def solve1(lines):
    s = 0 + 0j
    w = 1 + 0j
    for line in lines:
        nd = line[0]
        r = int(line[1:])
        if nd == "N":
            s += r * 1j
        elif nd == "S":
            s += r * -1j
        elif nd == "E":
            s += r * 1
        elif nd == "W":
            s += r * -1
        elif nd == "L":
            w *= 1j ** (r // 90)
        elif nd == "R":
            w *= (-1j) ** (r // 90)
        elif nd == "F":
            s += r * w

    return int(abs(s.real) + abs(s.imag))


def solve2(instructions):
    s = 0
    w = 10 + 1j
    for instruction in instructions:
        nd = instruction[0]
        r = int(instruction[1:])
        if nd == "N":
            w += r * 1j
        elif nd == "S":
            w += r * -1j
        elif nd == "E":
            w += r * 1
        elif nd == "W":
            w += r * -1
        elif nd == "L":
            w *= 1j ** (r // 90)
        elif nd == "R":
            w *= (-1j) ** (r // 90)
        elif nd == "F":
            s += r * w

    return int(abs(s.real) + abs(s.imag))


if __name__ == "__main__":
    with open("./input1.txt") as inputs:
        lines = list(inputs)

        # Warm-up
        for _ in range(100):
            solve1(lines)
            solve2(lines)

        t0 = time.time()
        solve1(lines)
        solve2(lines)
        t1 = time.time()
        print("Time:", t1 - t0)

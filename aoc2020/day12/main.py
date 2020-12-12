#!/usr/bin/env python


def solve1(lines):
    p = 0 + 0j
    d = 1 + 0j
    for line in lines:
        nd = line[0]
        r = int(line[1:])
        if nd == "N":
            p += r * 1j
        elif nd == "S":
            p += r * -1j
        elif nd == "E":
            p += r * 1
        elif nd == "W":
            p += r * -1
        elif nd == "L":
            d *= 1j ** (r // 90)
        elif nd == "R":
            d *= (-1j) ** (r // 90)
        elif nd == "F":
            p += r * d
    print("Part 1:", int(abs(p.real) + abs(p.imag)))


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

    print("Part 2:", int(abs(s.real) + abs(s.imag)))


if __name__ == "__main__":
    with open("./input1.txt") as inputs:
        lines = list(inputs)
        solve1(lines)
        solve2(lines)

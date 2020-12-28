#!/usr/bin/env python

from collections import Counter

OFFSETS = {
    "ne": (1, -1, 0),
    "e": (1, 0, -1),
    "se": (0, 1, -1),
    "sw": (-1, 1, 0),
    "w": (-1, 0, 1),
    "nw": (0, -1, 1),
}


def parse(line):
    while line:
        d = line[:2]
        if d in OFFSETS:
            yield OFFSETS[d]
            line = line[2:]
        else:
            yield OFFSETS[line[0]]
            line = line[1:]


def neighbors(coord):
    x, y, z = coord
    for dx, dy, dz in OFFSETS.values():
        yield x + dx, y + dy, z + dz


def coordinates():
    with open("./input1.txt") as inputs:
        for line in inputs:
            yield list(parse(line.strip()))


def tiles():
    s = set()
    for transformations in coordinates():
        x, y, z = 0, 0, 0
        for dx, dy, dz in transformations:
            x += dx
            y += dy
            z += dz

        coord = x, y, z
        if coord in s:
            s.remove(coord)
        else:
            s.add(coord)

    return s


def solve1():
    return len(tiles())


def solve2():
    black = set()
    counter = Counter()
    for transformations in coordinates():
        x, y, z = 0, 0, 0
        for dx, dy, dz in transformations:
            x += dx
            y += dy
            z += dz
            counter[(x, y, z)] = 0
            for neighbor in neighbors((x, y, z)):
                counter[neighbor] = 0
        coord = x, y, z
        if coord in black:
            black.remove(coord)
        else:
            black.add(coord)

    for coord in black:
        for neighbor in neighbors(coord):
            counter[neighbor] += 1

    for i in range(100):
        to_black = set()
        to_white = set()

        for coord, count in counter.items():
            if coord in black:
                if count == 0 or count > 2:
                    to_white.add(coord)
            elif count == 2:
                to_black.add(coord)

        for coord in to_black:
            black.add(coord)
            for neighbor in neighbors(coord):
                counter[neighbor] += 1

        for coord in to_white:
            black.remove(coord)
            for neighbor in neighbors(coord):
                counter[neighbor] -= 1

    return len(black)


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

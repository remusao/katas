#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys


def main():
    coordinates = []
    max_x, max_y = 0, 0
    for line in sys.stdin:
        x, y = map(int, line.strip().split(", "))
        coordinates.append(x + y * 1j)
        max_x = max(max_x, x)
        max_y = max(max_y, y)

    print(
        "Part 2:",
        sum(
            1
            for y in range(0, max_y + 1)
            for x in range(0, max_x + 1)
            if sum(abs(coord.real - x) + abs(coord.imag - y) for coord in coordinates)
            < 10000
        ),
    )


if __name__ == "__main__":
    main()

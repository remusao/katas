#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import sys


def read_input():
    velocities = []
    points = []
    for line in sys.stdin:
        x, y, vx, vy = map(int, re.findall(r"-?\d+", line))
        velocities.append(vx + vy * 1j)
        points.append(x + y * 1j)
    return points, velocities


def get_bounding_box(points):
    return (
        max(points, key=lambda p: p.real).real,
        max(points, key=lambda p: p.imag).imag,
        min(points, key=lambda p: p.real).real,
        min(points, key=lambda p: p.imag).imag,
    )


def draw(points):
    points = frozenset(points)
    max_x, max_y, min_x, min_y = get_bounding_box(points)
    xs = range(int(min_x), int(max_x) + 1)
    for y in range(int(min_y), int(max_y) + 1):
        print("".join("#" if (x + y * 1j) in points else "." for x in xs))


def main():
    points, velocities = read_input()

    seconds = 1
    while True:
        points = [points[i] + velocities[i] for i in range(len(points))]
        max_x, max_y, min_x, min_y = get_bounding_box(points)
        points_on_bbox = sum(
            1
            for p in points
            if p.real == max_x or p.real == min_x or p.imag == max_y or p.imag == min_y
        )

        if points_on_bbox > (len(points) / 3):
            print("Number of seconds", seconds)
            draw(points)
            return

        seconds += 1


if __name__ == "__main__":
    main()

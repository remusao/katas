#!/usr/bin/env python


def print_grid(grid):
    max_y = int(max(coord.imag for coord in grid))
    max_x = int(max(coord.real for coord in grid))

    for y in range(0, max_y + 1):
        for x in range(0, max_x + 1):
            if x + y * 1j in grid:
                print("#", end="")
            else:
                print(" ", end="")
        print()


def solve1():
    with open("./input.txt") as inputs:
        grid = set()
        points, folds = inputs.read().strip().split("\n\n")
        for line in points.split("\n"):
            x, y = map(int, line.split(","))
            grid.add(x + y * 1j)

        for fold in folds.split("\n"):
            fold = fold[len("fold along ") :]
            along, value = fold.split("=")
            value = int(value)
            if along == "x":
                grid = {
                    point
                    if point.real < value
                    else value - (point.real - value) + point.imag * 1j
                    for point in grid
                }
            elif along == "y":
                grid = {
                    point
                    if point.imag < value
                    else value * 1j - (point.imag - value) * 1j + point.real
                    for point in grid
                }

        print_grid(grid)
        print(len(grid))


if __name__ == "__main__":
    solve1()

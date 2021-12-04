import re


def last(iterator):
    last = None
    for elt in iterator:
        last = elt
    return last


def solve():
    with open("./input.txt") as inputs:
        numbers, raw = inputs.read().split("\n", 1)
        numbers = [int(number) for number in numbers.split(",")]

        grids = []
        for block in raw.split("\n\n"):
            grid = []
            for line in block.strip().split("\n"):
                grid.append([int(number) for number in re.split(r"\s+", line.strip())])
            for i in range(5):
                grid.append([line[i] for line in grid[:5]])
            grids.append([set(line_or_column) for line_or_column in grid])

        for number in numbers:
            for grid in grids:
                for line_or_column in grid:
                    line_or_column.discard(number)

                if any(not line_or_column for line_or_column in grid):
                    yield number * sum(n for line in grid[:5] for n in line)
                    grids = [g for g in grids if g is not grid]


def solve1():
    return next(solve())


def solve2():
    return last(solve())


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

    import time

    n = 100
    t0 = time.time()
    for _ in range(n):
        solve1()
        solve2()
    t1 = time.time()
    print("Total:", (t1 - t0) / n)

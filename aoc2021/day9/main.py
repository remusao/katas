#!/usr/bin/env python


def solve1():
    with open("./input.txt") as inputs:
        grid = {}
        for y, line in enumerate(inputs):
            for x, height in enumerate(line.strip()):
                grid[x + y * 1j] = int(height)

        result = 0
        for coord, height in grid.items():
            if height < min(
                grid[coord + delta]
                for delta in (1, -1, 1j, -1j)
                if coord + delta in grid
            ):
                result += height + 1
        return result


def solve2():
    with open("./input.txt") as inputs:
        grid = set()
        for y, line in enumerate(inputs):
            for x, height in enumerate(line.strip()):
                if height != "9":
                    grid.add(x + y * 1j)

        basins = []
        while grid:
            queue = {next(iter(grid))}
            basin = 0
            while queue:
                coord = queue.pop()
                grid.remove(coord)
                basin += 1
                queue.update(
                    (coord + delta)
                    for delta in (1, -1, 1j, -1j)
                    if (coord + delta) in grid
                )
            basins.append(basin)
        basins.sort(reverse=True)
        return basins[0] * basins[1] * basins[2]


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

#!/usr/bin/env python


def solve1():
    with open("./input.txt") as inputs:
        grid = {}
        for j, line in enumerate(inputs):
            for i, value in enumerate(line.strip()):
                grid[i + j * 1j] = int(value)

        deltas = (
            1,
            -1,
            1j,
            -1j,
            1 + 1j,
            1 - 1j,
            -1 + 1j,
            -1 - 1j,
        )

        total = 0
        step = 0
        while True:
            step += 1
            for coords in grid:
                grid[coords] += 1

            flashed = set()
            queue = [coords for coords, value in grid.items() if value > 9]
            while queue:
                coords = queue.pop()
                if coords in flashed:
                    continue
                flashed.add(coords)

                # Increase count around
                for delta in deltas:
                    if delta + coords in grid:
                        grid[coords + delta] += 1

                # Add newly flashed to list
                queue.extend(
                    [
                        coords + delta
                        for delta in deltas
                        if coords + delta in grid and grid[coords + delta] > 9
                    ]
                )

            total += len(flashed)
            if step == 100:
                print("Part 1:", total)

            if len(flashed) == len(grid):
                print("Part 2:", step)
                return

            for coords in flashed:
                grid[coords] = 0


if __name__ == "__main__":
    solve1()

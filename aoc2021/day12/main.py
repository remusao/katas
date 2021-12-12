#!/usr/bin/env python

from collections import defaultdict


def paths(current, maze, partial, allow_small_cave_reuse):
    if current == "end":
        yield partial
        return

    for candidate in maze[current]:
        if candidate == "start":
            continue

        if candidate.isupper() or (
            allow_small_cave_reuse
            and partial.count(candidate) < 2
            or candidate not in partial
        ):
            yield from paths(
                candidate,
                maze,
                partial + (candidate,),
                allow_small_cave_reuse=allow_small_cave_reuse
                and (candidate.isupper() or candidate not in partial),
            )


def solve(allow_small_cave_reuse):
    with open("./input.txt") as inputs:
        maze = defaultdict(list)
        for line in inputs:
            start, end = line.strip().split("-")
            maze[start].append(end)
            maze[end].append(start)

        count = 0
        for _ in paths("start", maze, ("start",), allow_small_cave_reuse):
            count += 1

        return count


def solve1():
    return solve(allow_small_cave_reuse=False)


def solve2():
    return solve(allow_small_cave_reuse=True)


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

#!/usr/bin/env python3

from collections import Counter


def main():
    # Part 1
    score = 0
    with open("./input.txt") as inputs:
        for line in inputs:
            line = line.strip()
            intersection = set(line[: len(line) // 2]).intersection(
                set(line[len(line) // 2 :])
            )
            assert len(intersection) == 1, intersection
            common = next(iter(intersection))
            if "a" <= common <= "z":
                score += ord(common) - ord("a") + 1
            else:
                score += ord(common) - ord("A") + 27
    print("Part 1:", score)

    # Part 2
    score = 0
    with open("./input.txt") as inputs:
        lines = [line.strip() for line in inputs]
        while lines:
            intersection = (
                set(lines.pop(0))
                .intersection(set(lines.pop(0)))
                .intersection(set(lines.pop(0)))
            )
            assert len(intersection) == 1, intersection
            common = next(iter(intersection))
            if "a" <= common <= "z":
                score += ord(common) - ord("a") + 1
            else:
                score += ord(common) - ord("A") + 27
    print("Part 2:", score)


if __name__ == "__main__":
    main()

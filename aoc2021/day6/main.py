#!/usr/bin/env python


MEMO = {}


def solve_rec(counter, n):
    if n == 0:
        return 1

    key = (counter, n)
    value = MEMO.get(key)
    if value is not None:
        return value

    value = (
        (solve_rec(8, n - 1) + solve_rec(6, n - 1))
        if counter == 0
        else solve_rec(counter - 1, n - 1)
    )
    MEMO[key] = value

    return value


def solve(fishes, n):
    memo = {i: solve_rec(i, n=n) for i in range(0, 9)}
    return sum(memo[counter] for counter in fishes)


def solve1():
    with open("./input.txt") as inputs:
        return solve([int(n) for n in inputs.read().strip().split(",")], n=80)


def solve2():
    with open("./input.txt") as inputs:
        return solve([int(n) for n in inputs.read().strip().split(",")], n=256)


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

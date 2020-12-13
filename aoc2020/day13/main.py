#!/usr/bin/env python


def solve2(lines):
    ts = [(i, int(t)) for i, t in enumerate(lines[1].strip().split(",")) if t != "x"]
    N = 1
    for _, t in ts:
        N *= t
    return sum((t - i) * N // t * pow(N // t, -1, t) for i, t in ts) % N


def solve1(lines):
    n = int(lines[0])
    ts = [int(t) for t in lines[1].strip().split(",") if t != "x"]
    m = min(ts)
    for i in range(n, n + m + 1):
        for t in ts:
            if i % t == 0:
                return (i - n) * t


def main():
    # Test suit
    for (inputs, expected) in [
        ("17,x,13,19", 3417),
        ("67,7,59,61", 754018),
        ("67,x,7,59,61", 779210),
        ("67,7,x,59,61", 1261476),
        ("1789,37,47,1889", 1202161486),
    ]:
        assert solve2([None, inputs]) == expected

    with open("./input1.txt") as inputs:
        lines = list(inputs)
        print("Part 1:", solve1(lines))
        print("Part 2:", solve2(lines))


if __name__ == "__main__":
    main()

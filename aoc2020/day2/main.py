#!/usr/bin/env python


def solve1(inputs):
    valid = 0
    for line in inputs:
        a, b = line.strip().split(": ", 1)
        r, l = a.split()
        mini, maxi = map(int, r.split("-"))
        count = 0
        for c in b:
            if c == l:
                count += 1

        if mini <= count <= maxi:
            valid += 1

        print(count, l, b)
    print(valid)


def solve2(inputs):
    valid = 0
    for line in inputs:
        a, b = line.strip().split(": ", 1)
        r, l = a.split()
        p1, p2 = map(int, r.split("-"))

        p1 -= 1
        p2 -= 1

        if p1 < len(b) and p2 < len(b) and (b[p1] == l) != (b[p2] == l):
            valid += 1

    print(valid)


def main():
    with open("./input1.txt") as inputs:
        solve2(inputs)


if __name__ == "__main__":
    main()

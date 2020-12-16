#!/usr/bin/env python

from functools import reduce
from collections import defaultdict


def solve1():
    # NOTES:
    # * Exactly two ranges per field.
    # * Bounds seem small enough that we can do a reverse index.
    with open("./input1.txt") as inputs:
        fields, mine, nearby = inputs.read().strip().split("\n\n")

        rfx = defaultdict(set)
        fx = {}
        for field in fields.split("\n"):
            name, ranges = field.split(": ")
            rx = []
            for r in ranges.split(" or "):
                a, b = map(int, r.split("-"))
                rx.append((a, b))
                for i in range(a, b + 1):
                    rfx[i].add(name)
            fx[name] = rx

        ticket = list(map(int, mine.split("\n")[-1].split(",")))
        tickets = [list(map(int, t.split(","))) for t in nearby.split("\n")[1:]]

        # print(rfx)
        # print(fx)
        # print(ticket)
        # print(tickets)
        res = 0
        for t in tickets:
            for i in t:
                if i not in rfx:
                    res += i

        print("Part 1:", res)


def solve2():
    with open("./input1.txt") as inputs:
        fields, mine, nearby = inputs.read().strip().split("\n\n")

        rfx = defaultdict(set)
        fx = {}
        for field in fields.split("\n"):
            name, ranges = field.split(": ")
            rx = []
            for r in ranges.split(" or "):
                a, b = map(int, r.split("-"))
                rx.append((a, b))
                for i in range(a, b + 1):
                    rfx[i].add(name)
            fx[name] = rx

        ticket = list(map(int, mine.split("\n")[-1].split(",")))
        tickets = [list(map(int, t.split(","))) for t in nearby.split("\n")[1:]]

        # print(rfx)
        # print(fx)
        # print(ticket)
        # print(tickets)
        valid = [t for t in tickets if all(i in rfx for i in t)]
        possible = list(
            enumerate(
                [reduce(lambda a, b: a & b, (rfx[i] for i in t)) for t in zip(*valid)]
            )
        )

        used = set()
        res = 1
        while possible:
            i, classes = next((i, p) for i, p in possible if len(p) == 1)
            name = next(iter(classes))
            if name.startswith("departure"):
                res *= ticket[i]
            # print(i, classes)
            used.update(classes)
            # print('used', used)
            possible = [(i, p - used) for i, p in possible if len(p - used) != 0]
            # print(possible)

        print("Part 2:", res)


if __name__ == "__main__":
    solve1()
    solve2()

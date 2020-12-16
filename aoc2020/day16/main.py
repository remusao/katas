#!/usr/bin/env python

import time
from functools import reduce
from collections import defaultdict


def parse(raw):
    fields, mine, nearby = raw.strip().split("\n\n")

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

    return rfx, ticket, tickets


def solve1(fields, _, nearby):
    res = 0

    for ticket in nearby:
        for value in ticket:
            if value not in fields:
                res += value

    return res


def solve2(fields, ticket, nearby):
    # Filter invalid tickets
    nearby = [ticket for ticket in nearby if all(value in fields for value in ticket)]

    # Get initial set of possible fields for each columns (e.g. {'row', 'seat'})
    columns = list(
        enumerate(
            (reduce(lambda a, b: a & b, (fields[i] for i in t)) for t in zip(*nearby))
        )
    )

    res = 1

    while columns:
        # Find next column which has only one possible field
        j, (i, classes) = next(
            ((j, (i, p)) for j, (i, p) in enumerate(columns) if len(p) == 1)
        )

        # Get name of field (Note: classes has only one value here)
        name = next(iter(classes))

        # Keep track of final result
        if name.startswith("departure"):
            res *= ticket[i]

        # Remove this column
        columns.pop(j)

        # Update all remaining columns by removing this field
        for _, classes in columns:
            classes.discard(name)

    return res


if __name__ == "__main__":
    with open("./input1.txt") as inputs:
        raw = inputs.read()
        fields, ticket, nearby = parse(raw)

        print("Part 1:", solve1(fields, ticket, nearby))
        print("Part 2:", solve2(fields, ticket, nearby))

        t0 = time.time()
        solve1(fields, ticket, nearby)
        solve2(fields, ticket, nearby)
        t1 = time.time()
        print("Total:", t1 - t0)

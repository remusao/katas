#!/usr/bin/env python

from functools import reduce
from collections import Counter, defaultdict


def solve(raw):
    counter = Counter()
    allergens = defaultdict(list)
    for line in raw.splitlines():
        if " (contains " in line:
            ingr, al = line.strip().split(" (contains ")
            ingr = ingr.strip().split()
            for a in al.strip()[:-1].split(", "):
                allergens[a].append(ingr)
            counter.update(ingr)
        else:
            counter.update(ingr.strip().split())

    with_allergens = []
    allergens = {
        allergen: reduce(set.intersection, map(set, ingredients))
        for allergen, ingredients in allergens.items()
    }

    while allergens:
        a, ix = next((a, ix) for a, ix in allergens.items() if len(ix) == 1)
        i = next(iter(ix))
        with_allergens.append((i, a))
        del allergens[a]
        for ix in allergens.values():
            ix.discard(i)

    return counter, with_allergens


def solve1(raw):
    counter, allergens = solve(raw)
    with_allergens = {i for i, _ in allergens}

    result = 0
    for i, c in counter.items():
        if i not in with_allergens:
            result += c
    return result


def solve2(raw):
    _, allergens = solve(raw)
    allergens.sort(key=lambda a: a[1])
    return ",".join(a for a, _ in allergens)


if __name__ == "__main__":
    with open("./input1.txt") as inputs:
        raw = inputs.read()
        print("Part 1:", solve1(raw))
        print("Part 2:", solve2(raw))

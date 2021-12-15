#!/usr/bin/env python

from collections import Counter


class Node:
    __slots__ = ("value", "prev", "next")

    def __init__(self, value):
        self.value = value
        self.prev = None
        self.next = None


def iterll(root):
    while root:
        yield root
        root = root.next


def pprint(root):
    for node in iterll(root):
        print(node.value, end="")
    print()


def solve1():
    with open("./input.txt") as inputs:
        start, raw_rules = inputs.read().strip().split("\n\n")
        rules = {}
        for raw_rule in raw_rules.strip().split("\n"):
            pattern, inserted = raw_rule.split(" -> ")
            rules[tuple(pattern)] = inserted

        root = Node(start[0])
        current = root
        for value in start[1:]:
            new = Node(value)
            new.prev = current
            current.next = new
            current = new

        for _ in range(10):
            current = root
            while current.next:
                v1 = current.value
                v2 = current.next.value
                insert = rules.get((v1, v2))
                if insert is not None:
                    new = Node(insert)
                    new.prev = current
                    new.next = current.next
                    current.next = new
                    current.next.prev = new
                    current = new.next
                else:
                    current = current.next

            # pprint(root)

        counter = Counter()
        for node in iterll(root):
            counter[node.value] += 1
        values = sorted(counter.values())
        return values[-1] - values[0]


def solve2():
    with open("./input.txt") as inputs:
        start, raw_rules = inputs.read().strip().split("\n\n")
        rules = {}
        for raw_rule in raw_rules.strip().split("\n"):
            pattern, inserted = raw_rule.split(" -> ")
            rules[tuple(pattern)] = inserted

        pairs = Counter()
        for i, v in enumerate(start):
            if i + 1 < len(start):
                pairs[(start[i], start[i + 1])] += 1
        pairs[("0", start[0])] = 1
        pairs[(start[-1], "0")] = 1

        for step in range(40):
            new = Counter(pairs)
            for pair, count in pairs.items():
                inserted = rules.get(pair)
                if inserted:
                    new[pair] -= count
                    new[(pair[0], inserted)] += count
                    new[(inserted, pair[1])] += count
            pairs = new

            # print(pairs)
            counter = Counter()
            for key, value in pairs.items():
                for letter in key:
                    if letter != "0":
                        counter[letter] += value
            values = sorted(counter.values())
        return int(values[-1] / 2 - values[0] / 2)


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

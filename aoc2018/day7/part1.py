#!/usr/bin/env python
# -*- coding: utf-8 -*-

from collections import defaultdict
import sys


def main():
    steps = set()
    constraints = defaultdict(set)
    reverse = defaultdict(set)
    for line in sys.stdin:
        dep, step = line[5], line[36]
        steps.add(dep)
        steps.add(step)
        constraints[step].add(dep)
        reverse[dep].add(step)

    output = ""
    while steps:
        # print(output, steps)
        not_constrained = min(steps.difference(constraints), key=ord)
        # print(not_constrained)
        output += "".join(not_constrained)
        for constraining in reverse[not_constrained]:
            # print(">>", constraining)
            constraints[constraining].remove(not_constrained)
            if not constraints[constraining]:
                del constraints[constraining]
        del reverse[not_constrained]
        steps.remove(not_constrained)
    print(f"Part 1: {output}")


if __name__ == "__main__":
    main()

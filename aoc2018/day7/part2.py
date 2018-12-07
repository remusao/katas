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

    workers = 5
    next_termination = []
    t = 0
    time_per_step = 60
    progress = set()
    while steps:
        # print(output, steps)
        print("T=", t)
        not_constrained = sorted(steps.difference(constraints).difference(progress))
        print(not_constrained)
        for step in not_constrained:
            if workers == 0:
                break

            print("Step", step)

            workers -= 1
            progress.add(step)
            next_termination.append(
                (t + time_per_step + ord(step) - ord("A") + 1, step)
            )
            print("Worker finishes at", next_termination[-1])

        next_termination.sort(reverse=True)
        t = next_termination[-1][0]
        while next_termination and next_termination[-1][0] == t:
            _, step = next_termination.pop()
            print("Finished", step)
            workers += 1

            for constraining in reverse[step]:
                constraints[constraining].remove(step)
                if not constraints[constraining]:
                    del constraints[constraining]

            del reverse[step]
            steps.remove(step)
            progress.remove(step)

    if next_termination:
        print("Remaining?", t, next_termination)
        t = max(t, *[term for term, _ in next_termination])
    print(f"Part 2: {t}")


if __name__ == "__main__":
    main()

#!/usr/bin/env python

def rec(d1, d2):
    seen = set()
    while d1 and d2:
        # Keep track of seen decks
        t = tuple(d1) + ((),) + tuple(d2)
        if t in seen:
            return 1, d1, d2
        seen.add(t)

        # Draw a card
        c1 = d1.pop(0)
        c2 = d2.pop(0)
        assert c1 != c2

        if len(d1) >= c1 and len(d2) >= c2:
            winner, d1_, d2_ = rec(d1[:c1], d2[:c2])
            if winner == 1:
                d1.append(c1)
                d1.append(c2)
            else:
                d2.append(c2)
                d2.append(c1)
        else:
            if c1 > c2:
                d1.append(c1)
                d1.append(c2)
            else:
                d2.append(c2)
                d2.append(c1)

    if d1:
        return 1, d1, d2

    return 2, d1, d2


def solve2():
    with open('./input1.txt') as inputs:
        p1, p2 = inputs.read().strip().split('\n\n')
        d1 = [int(n) for n in p1.split('\n')[1:]]
        d2 = [int(n) for n in p2.split('\n')[1:]]

        winner, d1_, d2_ = rec(d1, d2)

        score = 0
        d = d1_ or d2_
        for i, c in enumerate(d[::-1]):
            score += (i + 1) * c

        print('Part 2:', score)


def solve1():
    with open('./input1.txt') as inputs:
        p1, p2 = inputs.read().strip().split('\n\n')
        d1 = [int(n) for n in p1.split('\n')[1:]]
        d2 = [int(n) for n in p2.split('\n')[1:]]

        while d1 and d2:
            c1 = d1.pop(0)
            c2 = d2.pop(0)
            assert c1 != c2
            if c1 > c2:
                d1.append(c1)
                d1.append(c2)
            else:
                d2.append(c2)
                d2.append(c1)

        score = 0
        d = d1 or d2
        for i, c in enumerate(d[::-1]):
            score += (i + 1) * c

        print('Part 1:', score)


if __name__ == "__main__":
    solve1()
    solve2()

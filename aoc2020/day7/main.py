
import functools
from collections import defaultdict, Counter

def norm(bag):
    bag = bag.strip()
    if bag.endswith(' bag'):
        bag = bag[:-4]
    if bag.endswith(' bags'):
        bag = bag[:-5]
    return bag


MEMO = {}


def rec(m, b):
    # print('?', b)
    res = 1
    for (n, bag) in m[b]:
        # print(' >', n, bag)

        bag_n = MEMO.get(bag)
        if bag_n is None:
            bag_n = rec(m, bag)
            MEMO[bag] = bag_n

        res += n * bag_n

    return res


def solve2():
    with open('./input1.txt') as inputs:
        m = defaultdict(list)
        for line in inputs:
            ba, b = line.strip().split(' contain ')
            ba = norm(ba)
            bs = b[:-1].split(', ')
            # print('+', ba)
            for bag in bs:
                if bag != 'no other bags':
                    n, bag = bag.split(' ', 1)
                    n = int(n)
                    bag = norm(bag)
                    # print(' ', n, bag)
                    m[ba].append((n, bag))

        print(rec(m, 'shiny gold') - 1)



def solve1():
    with open('./input1.txt') as inputs:
        m = defaultdict(set)
        for line in inputs:
            ba, b = line.strip().split(' contain ')
            ba = norm(ba)
            bs = b[:-1].split(', ')
            # print('+', ba)
            for bag in bs:
                if bag != 'no other bags':
                    _, bag = bag.split(' ', 1)
                    bag = norm(bag)
                    m[bag].add(ba)
                    # print(' ', bag)

        r = set()
        q = ['shiny gold']
        while q:
            c = q.pop(0)
            if c not in r:
                r.add(c)
                q.extend(m[c])

        print(len(r) - 1)


if __name__ == "__main__":
    solve1()
    solve2()


from collections import Counter
import re

def solve1():
    with open('./input1.txt') as inputs:
        res = 0
        for group in inputs.read().strip().split('\n\n'):
            res += len(set(re.compile(r'\s+').sub('', group)))
        print(res)


def solve2():
    with open('./input1.txt') as inputs:
        res = 0
        for group in inputs.read().strip().split('\n\n'):
            persons = len(group.split('\n'))
            count = Counter(re.compile(r'\s+').sub('', group))
            for c, cnt in count.items():
                if cnt == persons:
                    res += 1
        print(res)


if __name__ == "__main__":
    solve1()
    solve2()

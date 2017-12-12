
from collections import defaultdict
import sys


def load_programs():
    s = defaultdict(set)
    for line in sys.stdin.read().splitlines():
        source, dest = line.split(' <-> ')
        source = int(source.strip())
        programs = [int(p.strip()) for p in dest.split(',')]

        s[source].update(programs)
        for p in programs:
            s[p].add(source)
    return s

def solve2():
    programs = load_programs()
    groups = 0
    while programs:
        p1 = next(iter(programs))
        while True:
            s2 = set(programs[p1])
            for p in programs[p1]:
                s2.update(programs[p])

            if len(s2) == len(programs[p1]):
                for p in programs[p1]:
                    del programs[p]
                if p1 in programs:
                    del programs[p1]
                groups += 1
                break
            programs[p1] = s2
    print(groups)

def solve1():
    programs = load_programs()
    while True:
        s2 = set(programs[0])
        for p in programs[0]:
            s2.update(programs[p])
        if len(s2) == len(programs[0]):
            print(len(s2))
            return
        programs[0] = s2

def main():
    solve2()


if __name__ == '__main__':
    main()

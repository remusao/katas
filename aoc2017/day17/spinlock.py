
import sys


def solve1(steps):
    buff = [0]
    pos = 0
    for i in range(2017):
        pos = (pos + steps) % len(buff) + 1
        buff.insert(pos, i + 1)
    print(buff[(pos + 1) % len(buff)])

def solve2(steps):
    pos, res = 0, 0
    for i in range(50000000):
        pos = (pos + steps) % (i + 1) + 1
        if pos == 1:
            res = i + 1
    print(res)

def main():
    n = int(sys.stdin.read().strip())
    solve1(n)
    solve2(n)

if __name__ == '__main__':
    main()

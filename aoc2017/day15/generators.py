
import sys
from itertools import islice, izip

def generate(start, factor):
    while True:
        start = (start * factor) % 2147483647
        yield start

def solve1(startA, startB, n):
    genA = generate(startA, 16807)
    genB = generate(startB, 48271)

    return sum(
        1
        for (a, b) in islice(izip(genA, genB), n)
        if (a & 65535) == (b & 65535)
    )

def solve2(startA, startB, n):
    genA = (a for a in generate(startA, 16807) if a % 4 == 0)
    genB = (b for b in generate(startB, 48271) if b % 8 == 0)

    return sum(
        1
        for (a, b) in islice(izip(genA, genB), n)
        if (a & 65535) == (b & 65535)
    )

def main():
    a, b = sys.stdin.read().splitlines()
    startA = int(a[24:])
    startB = int(b[24:])

    print(solve1(
        startA=startA,
        startB=startB,
        n=40000000
    ))

    print(solve2(
        startA=startA,
        startB=startB,
        n=5000000
    ))

if __name__ == '__main__':
    main()

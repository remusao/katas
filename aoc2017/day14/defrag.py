import sys
from functools import reduce
from operator import xor

def hash_round(array, lengths, pos=0, skip=0):
    length = len(array)
    for l in lengths:
        for i in range(l // 2):
            b = (pos + i) % length
            e = (pos + l - i - 1) % length
            array[b], array[e] = array[e], array[b]
        pos = (pos + l + skip) % length
        skip += 1
    return array, pos, skip

def knot_hash(str):
    array = list(range(256))
    lengths = [ord(c) for c in str] + [17, 31, 73, 47, 23]

    pos, skip = 0, 0
    for _ in range(64):
        array, pos, skip = hash_round(array, lengths, pos=pos, skip=skip)

    return ''.join(
        '{0:02x}'.format(reduce(xor, array[i:i + 16]))
        for i in range(0, len(array), 16)
    )

def create_grid():
    str = sys.stdin.read().strip()
    for i in range(128):
        hex = knot_hash('{str}-{i}'.format(
            str=str,
            i=i
        ))
        yield '{:0128b}'.format(int(hex, 16))

def solve1(grid):
    total = 0
    for line in grid:
        total += line.count('1')
    print(total)

def solve2(grid):
    coords = set()
    for y, line in enumerate(grid):
        for x, v in enumerate(line):
            if v == '1':
                coords.add((x, y))
    n = 0
    while coords:
        group = [coords.pop()]
        n += 1
        while group:
            x, y = group.pop()
            adj = []
            if (x + 1, y) in coords:
                adj.append((x + 1, y))
            if (x, y + 1) in coords:
                adj.append((x, y + 1))
            if (x - 1, y) in coords:
                adj.append((x - 1, y))
            if (x, y - 1) in coords:
                adj.append((x, y - 1))
            for a in adj:
                coords.remove(a)
            group.extend(adj)
    print(n)

def main():
    solve2(create_grid())

if __name__ == '__main__':
    main()

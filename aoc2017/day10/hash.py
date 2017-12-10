

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

def solve1():
    array = list(range(256))
    lengths = [int(l.strip()) for l in sys.stdin.read().split(',')]
    hashed, _, _ = hash_round(array, lengths)
    return hashed[0] * hashed[1]

def solve2():
    array = list(range(256))
    lengths = [ord(c) for c in sys.stdin.read().strip()] + [17, 31, 73, 47, 23]

    pos, skip = 0, 0
    for _ in range(64):
        array, pos, skip = hash_round(array, lengths, pos=pos, skip=skip)

    return ''.join(
        '{0:02x}'.format(reduce(xor, array[i:i + 16]))
        for i in range(0, len(array), 16)
    )

def main():
    print(solve2())


if __name__ == '__main__':
    main()

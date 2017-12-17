
import sys


def solve1(steps):
    buff = [0]
    pos = 0
    for i in range(2017):
        pos = (pos + steps) % len(buff) + 1
        buff.insert(pos, i + 1)
    return buff[(pos + 1) % len(buff)]

def solve2(steps):
    pos, res = 0, 0
    i = 0
    n = 50000000
    while i < n:
        # How many times can we move `steps` times forward, before the end of
        # the buffer? The last position of the buffer being `i`, we want to
        # know the value of k such that: k * steps + k <= i - pos.
        # Since at each step we will add one element to the buffer, we need to
        # add `k` on the left-hand side of the equation.
        k = (i - pos) // (steps + 1)
        # We increment the current size of the buffer by `k` (+ 1 for the last
        # move we do which requires wrapping.
        i += k + 1
        # Updated position is: (pos + k * steps + k) % (i = new size)
        pos = (pos + (k + 1) * steps + k) % i + 1
        if pos == 1:
            res = i
    return res

def main():
    n = int(sys.stdin.read().strip())
    print(solve1(n))
    print(solve2(n))

if __name__ == '__main__':
    main()

from collections import Counter
import functools


def solve1():
    with open("./input1.txt") as inputs:
        counter = Counter({3: 1})
        current = 0
        for n in sorted(map(int, inputs)):
            if n - current <= 3:
                counter[n - current] += 1
                current = n

        print("Part 1:", counter[1] * counter[3])


def cache(f):
    memo = {}
    @functools.wraps(f)
    def cached(idx, current):
        m = memo.get(idx)
        if m is not None:
            return m

        m = f(idx, current)
        memo[idx] = m
        return m
    return cached


def solve2():
    with open("./input1.txt") as inputs:
        numbers = sorted(map(int, inputs))

        @cache
        def rec(idx, current):
            if idx == len(numbers):
                return 1

            return sum(
                rec(idx + 1 + i, n)
                for i, n in enumerate(numbers[idx : idx + 3])
                if n - current <= 3
            )

        print("Part 2:", rec(0, 0))



if __name__ == "__main__":
    solve1()
    solve2()

from collections import Counter


def solve1():
    with open("./input1.txt") as inputs:
        counter = Counter({3: 1})
        current = 0
        for n in sorted(map(int, inputs)):
            if n - current <= 3:
                counter[n - current] += 1
                current = n

        print("Part 1:", counter[1] * counter[3])


MEMO = {}


def solve2_rec(numbers, idx, current):
    if idx == len(numbers):
        return 1

    res = 0
    for i, n in enumerate(numbers[idx : idx + 3]):
        if n - current <= 3:
            new_idx = idx + 1 + i

            m = MEMO.get((new_idx, n))
            if m is not None:
                res += m
            else:
                m = solve2_rec(numbers, new_idx, n)
                MEMO[(new_idx, n)] = m
                res += m

    return res


def solve2():
    with open("./input1.txt") as inputs:
        print("Part 2:", solve2_rec(sorted(map(int, inputs)), 0, 0))


if __name__ == "__main__":
    solve1()
    solve2()

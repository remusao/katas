def run(numbers, stop):
    last_seen = {}
    for i, n in enumerate(numbers[:-1]):
        last_seen[n] = i + 1

    # Start with last number in the sequence
    i = len(numbers)
    n0 = numbers[-1]
    l = last_seen.get(n0, None)
    last_seen[n0] = i

    # Start computing numbers of the sequence
    while True:
        if l is None:
            n0 = 0
        else:
            n0 = i - l

        i += 1
        l = last_seen.get(n0)
        last_seen[n0] = i

        if i == stop:
            # print(len(last_seen))
            # print(min(last_seen))
            # print(max(last_seen))
            return n0


def solve1(numbers):
    return run(numbers, 2020)


def solve2(numbers):
    return run(numbers, 30000000)


if __name__ == "__main__":
    inputs = "0,3,6"
    inputs = "9,12,1,4,17,0,18"
    numbers = list(map(int, inputs.split(",")))

    print("Part 1:", solve1(numbers))
    print("Part 2:", solve2(numbers))

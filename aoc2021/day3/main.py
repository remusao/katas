from collections import Counter, defaultdict


def solve1():
    with open("./input.txt") as inputs:
        counters = defaultdict(Counter)
        for line in inputs:
            for i, c in enumerate(line.strip()):
                counters[i][c] += 1

        gamma = ""
        epsilon = ""
        for counter in counters.values():
            if counter["0"] > counter["1"]:
                gamma += "0"
                epsilon += "1"
            elif counter["1"] > counter["0"]:
                gamma += "1"
                epsilon += "0"
            else:
                assert False, "Counts cannot be equal"

        return int(gamma, 2) * int(epsilon, 2)


def solve2():
    with open("./input.txt") as inputs:
        values = [line.strip() for line in inputs]

        n = 0
        remaining = list(values)
        while len(remaining) != 1:
            counter = Counter()
            for value in remaining:
                counter[value[n]] += 1
            most_common = "0" if counter["0"] > counter["1"] else "1"

            remaining = [value for value in remaining if value[n] == most_common]
            n += 1
        oxygen = int(remaining[0], 2)

        n = 0
        remaining = list(values)
        while len(remaining) != 1:
            counter = Counter()
            for value in remaining:
                counter[value[n]] += 1
            most_common = "1" if counter["1"] < counter["0"] else "0"

            remaining = [value for value in remaining if value[n] == most_common]
            n += 1
        co2 = int(remaining[0], 2)

        return co2 * oxygen


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

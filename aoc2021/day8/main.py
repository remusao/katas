#!/usr/bin/env python


def solve1():
    with open("./input.txt") as inputs:
        result = 0
        for line in inputs:
            for value in line.strip().split("|", 1)[-1].strip().split():
                if len(value) in (2, 3, 4, 7):
                    result += 1
        return result


def solve2_impl(numbers, mapping, normal):
    for a in mapping["a"]:
        for b in mapping["b"]:
            if b in (a,):
                continue

            for c in mapping["c"]:
                if c in (a, b):
                    continue

                for d in mapping["d"]:
                    if d in (a, b, c):
                        continue

                    for e in mapping["e"]:
                        if e in (a, b, c, d):
                            continue

                        for f in mapping["f"]:
                            if f in (a, b, c, d, e):
                                continue

                            for g in mapping["g"]:
                                if g in (a, b, c, d, e, f):
                                    continue

                                found = True
                                result = ""
                                for number in numbers:
                                    converted = "".join(
                                        sorted(
                                            number.translate(
                                                str.maketrans(
                                                    "".join([a, b, c, d, e, f, g]),
                                                    "abcdefg",
                                                )
                                            )
                                        )
                                    )
                                    if converted not in normal:
                                        found = False
                                        break
                                    result += normal[converted]

                                if found is True:
                                    return int(result)
    assert False


def solve2():
    all_letter = set("abcdefg")
    normal = {
        "abcefg": "0",
        "cf": "1",
        "acdeg": "2",
        "acdfg": "3",
        "bcdf": "4",
        "abdfg": "5",
        "abdefg": "6",
        "acf": "7",
        "abcdefg": "8",
        "abcdfg": "9",
    }

    total = 0
    with open("./input.txt") as inputs:
        for line in inputs:
            mapping = {
                "a": set("abcdefg"),
                "b": set("abcdefg"),
                "c": set("abcdefg"),
                "d": set("abcdefg"),
                "e": set("abcdefg"),
                "f": set("abcdefg"),
                "g": set("abcdefg"),
            }
            left, right = line.strip().split("|", 1)
            values = left.strip().split()
            numbers = right.strip().split()
            for value in values:
                if len(value) == 2:  # 1
                    for v in "cf":
                        mapping[v] &= set(value)
                    for v in all_letter - set("cf"):
                        mapping[v] -= set(value)
                elif len(value) == 3:  # 7
                    for v in "acf":
                        mapping[v] &= set(value)
                    for v in all_letter - set("acf"):
                        mapping[v] -= set(value)
                elif len(value) == 4:  # 4
                    for v in "bcdf":
                        mapping[v] &= set(value)
                    for v in all_letter - set("bcdf"):
                        mapping[v] -= set(value)
            total += solve2_impl(mapping=mapping, normal=normal, numbers=numbers)

    return total


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

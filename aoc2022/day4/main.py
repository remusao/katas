#!/usr/bin/env python3


def main():
    # Part 1
    result = 0
    with open("./input.txt") as inputs:
        for line in inputs:
            elf1, elf2 = [
                list(map(int, elf.split("-"))) for elf in line.strip().split(",")
            ]
            assert elf1[0] <= elf1[1], elf1
            assert elf2[0] <= elf2[1], elf2
            if elf1[0] >= elf2[0] and elf1[0] <= elf2[1] and elf1[1] <= elf2[1]:
                result += 1
            elif elf2[0] >= elf1[0] and elf2[0] <= elf1[1] and elf2[1] <= elf1[1]:
                result += 1
    print("Part 1:", result)

    # Part 2
    result = 0
    with open("./input.txt") as inputs:
        for line in inputs:
            elf1, elf2 = [
                list(map(int, elf.split("-"))) for elf in line.strip().split(",")
            ]
            assert elf1[0] <= elf1[1], elf1
            assert elf2[0] <= elf2[1], elf2
            if elf2[1] >= elf1[0] >= elf2[0] or elf2[1] >= elf1[1] >= elf2[0]:
                result += 1
            elif elf1[1] >= elf2[0] >= elf1[0] or elf1[1] >= elf2[1] >= elf1[0]:
                result += 1
    print("Part 2:", result)


if __name__ == "__main__":
    main()

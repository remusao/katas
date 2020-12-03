def read_map(filename):
    with open(filename) as inputs:
        coords = set()
        lines = [line.strip() for line in inputs]
        for i, line in enumerate(lines):
            for j, x in enumerate(line):
                if x == "#":
                    coords.add((i, j))
    return len(lines), len(lines[0]), coords


def count_trees(max_i, max_j, coords, inc_i, inc_j):
    # i = vertical
    # 0
    # 1
    # 2
    # etc.

    # j = horizontal
    # 0 1 2 3 4 5

    i = 0
    j = 0
    count = 0
    while i < max_i:
        i += inc_i
        j = (j + inc_j) % max_j
        if (i, j) in coords:
            count += 1

    return count


def solve1(max_i, max_j, coords):
    print(
        "Part 1:",
        count_trees(max_i=max_i, max_j=max_j, coords=coords, inc_i=1, inc_j=3),
    )


def solve2(max_i, max_j, coords):
    result = 1
    for (inc_j, inc_i) in [
        (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2),
    ]:
        result *= count_trees(
            max_i=max_i, max_j=max_j, coords=coords, inc_i=inc_i, inc_j=inc_j
        )

    print("Part 2:", result)


def main():
    max_i, max_j, coords = read_map("./example.txt")
    max_i, max_j, coords = read_map("./input1.txt")

    solve1(max_i=max_i, max_j=max_j, coords=coords)
    solve2(max_i=max_i, max_j=max_j, coords=coords)


if __name__ == "__main__":
    main()

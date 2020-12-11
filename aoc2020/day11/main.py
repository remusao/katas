def pprint(max_i, max_j, empty, taken):
    for i in range(max_i):
        for j in range(max_j):
            if (i, j) in taken:
                print("#", end="")
            elif (i, j) in empty:
                print("L", end="")
            else:
                print(".", end="")
        print()


def parse_input():
    # i = vertical
    # 0
    # 1
    # 2
    # etc.

    # j = horizontal
    # 0 1 2 3 4 5
    with open("./input1.txt") as inputs:
        empty = set()
        taken = set()

        lines = [line.strip() for line in inputs]
        for i, line in enumerate(lines):
            for j, x in enumerate(line):
                if x == "L":
                    empty.add((i, j))
                elif x == "#":
                    taken.add((i, j))

        max_i = len(lines)
        max_j = len(lines[0])

        return (max_i, max_j, empty, taken)


DELTAS = (
    (-1, 0),
    (-1, -1),
    (-1, +1),
    (0, -1),
    (0, +1),
    (+1, 0),
    (+1, -1),
    (+1, +1),
)


def gen_next_part1(i, j, *args, **kwargs):
    for (di, dj) in DELTAS:
        yield (i + di, j + dj)


def gen_next_part2(i, j, max_i, max_j, empty, taken):
    for (di, dj) in DELTAS:
        ni, nj = i, j
        while ni >= 0 and ni < max_i and nj >= 0 and nj < max_j:
            ni += di
            nj += dj
            if (ni, nj) in empty or (ni, nj) in taken:
                yield (ni, nj)
                break


def run(max_i, max_j, empty, taken, neighbors, max_neighbors):
    changed = True
    while changed:
        changed = False
        new_empty = set()
        new_taken = set()

        for (i, j) in empty:
            if all(
                new not in taken
                for new in neighbors(
                    i=i, j=j, max_i=max_i, max_j=max_j, empty=empty, taken=taken
                )
            ):
                changed = True
                new_taken.add((i, j))
            else:
                new_empty.add((i, j))

        for (i, j) in taken:
            n = sum(
                new in taken
                for new in neighbors(
                    i=i, j=j, max_i=max_i, max_j=max_j, empty=empty, taken=taken
                )
            )
            if n >= max_neighbors:
                changed = True
                new_empty.add((i, j))
            else:
                new_taken.add((i, j))

        taken = new_taken
        empty = new_empty

    return len(taken)


def solve1():
    max_i, max_j, empty, taken = parse_input()
    print(
        "Part 1:",
        run(
            max_i=max_i,
            max_j=max_j,
            empty=empty,
            taken=taken,
            neighbors=gen_next_part1,
            max_neighbors=4,
        ),
    )


def solve2():
    max_i, max_j, empty, taken = parse_input()
    print(
        "Part 2:",
        run(
            max_i=max_i,
            max_j=max_j,
            empty=empty,
            taken=taken,
            neighbors=gen_next_part2,
            max_neighbors=5,
        ),
    )


if __name__ == "__main__":
    solve1()
    solve2()

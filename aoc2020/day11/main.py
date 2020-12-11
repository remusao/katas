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


def gen_next_part1(i, j, *args, **kwargs):
    for (di, dj) in (
        (-1, 0),
        (-1, -1),
        (-1, +1),
        (0, -1),
        (0, +1),
        (+1, 0),
        (+1, -1),
        (+1, +1),
    ):
        yield (i + di, j + dj)


def gen_next_part2(i, j, max_i, max_j, empty, taken):
    for (di, dj) in (
        (-1, 0),
        (-1, -1),
        (-1, +1),
        (0, -1),
        (0, +1),
        (+1, 0),
        (+1, -1),
        (+1, +1),
    ):
        ni, nj = i, j
        while ni >= 0 and ni < max_i and nj >= 0 and nj < max_j:
            ni += di
            nj += dj
            if (ni, nj) in empty or (ni, nj) in taken:
                yield (ni, nj)
                break


def solve1():
    with open("./input1.txt") as inputs:
        # i = vertical
        # 0
        # 1
        # 2
        # etc.

        # j = horizontal
        # 0 1 2 3 4 5

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

        changed = True
        while changed:
            # print()
            # pprint(len(lines), len(lines[0]), empty=empty, taken=taken)
            # if len(taken) == 6:
            #     break

            changed = False
            new_empty = set()
            new_taken = set()

            for (i, j) in empty:
                # if i == 9 and j == 0:
                #     print('!!!', all((i + di, j + dj) not in taken for (di, dj) in coords))
                if all(
                    new not in taken
                    for new in gen_next_part1(
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
                    for new in gen_next_part1(
                        i=i, j=j, max_i=max_i, max_j=max_j, empty=empty, taken=taken
                    )
                )
                if n >= 4:
                    changed = True
                    new_empty.add((i, j))
                else:
                    new_taken.add((i, j))

            taken = new_taken
            empty = new_empty

        print("Part 1:", len(taken))


def solve2():
    with open("./input1.txt") as inputs:
        # i = vertical
        # 0
        # 1
        # 2
        # etc.

        # j = horizontal
        # 0 1 2 3 4 5

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

        changed = True
        while changed:
            changed = False
            new_empty = set()
            new_taken = set()

            for (i, j) in empty:
                if all(
                    new not in taken
                    for new in gen_next_part2(
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
                    for new in gen_next_part2(
                        i=i, j=j, max_i=max_i, max_j=max_j, empty=empty, taken=taken
                    )
                )
                if n >= 5:  # TODO - parameterize
                    changed = True
                    new_empty.add((i, j))
                else:
                    new_taken.add((i, j))

            taken = new_taken
            empty = new_empty

        print("Part 2:", len(taken))


if __name__ == "__main__":
    solve1()
    solve2()

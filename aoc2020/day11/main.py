import array
import time


def pprint(max_i, max_j, empty, coords):
    coords = frozenset(coords)
    for i in range(max_i):
        for j in range(max_j):
            c = i * max_j + j
            if (i, j) not in coords:
                print(".", end="")
            elif empty[c] == 1:
                print("L", end="")
            else:
                print("#", end="")
        print()


def parse_input(lines):
    # i = vertical
    # 0
    # 1
    # 2
    # etc.

    # j = horizontal
    # 0 1 2 3 4 5
    max_i = len(lines)
    max_j = len(lines[0])

    coords = []
    empty = array.array("B", [0] * (max_i * max_j))

    for i, line in enumerate(lines):
        for j, x in enumerate(line):
            if x == "L":
                empty[i * max_j + j] = 1
                coords.append((i, j))
    ccoords = [i * max_j + j for (i, j) in coords]

    return (max_i, max_j, empty, coords, ccoords)


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


def gen_next_part1(i, j, max_i, max_j, empty):
    res = []
    for (di, dj) in DELTAS:
        ni = i + di
        nj = j + dj
        if ni >= 0 and ni < max_i and nj >= 0 and nj < max_j:
            c = (i + di) * max_j + (j + dj)
            if empty[c] == 1:
                res.append(c)
    return res


def gen_next_part2(i, j, max_i, max_j, empty):
    res = []
    for (di, dj) in DELTAS:
        ni, nj = i + di, j + dj
        while ni >= 0 and ni < max_i and nj >= 0 and nj < max_j:
            c = ni * max_j + nj
            if empty[c] == 1:
                res.append(c)
                break
            ni += di
            nj += dj
    return res


def run(max_i, max_j, empty, coords, ccoords, neighbors, max_neighbors):
    precompneighbors = [[]] * (max_i * max_j)
    for (i, j), c in zip(coords, ccoords):
        precompneighbors[c] = neighbors(i=i, j=j, max_i=max_i, max_j=max_j, empty=empty)

    # At first iteration no cell has any occupied neighbor => count is 0
    counts = empty
    for c in ccoords:
        counts[c] = 0

    previously_updated = ccoords
    update = 1  # First iteration is about adding
    occupied = 0  # Total number of seats occupied

    while len(previously_updated) != 0:
        to_update = (
            [c for c in previously_updated if counts[c] == 0]
            if update == 1
            else [c for c in previously_updated if counts[c] >= max_neighbors]
        )

        if len(to_update) != 0:
            for c in to_update:
                for n in precompneighbors[c]:
                    counts[n] += update

            occupied += update * len(to_update)

        previously_updated = to_update
        update = -1 if update == 1 else 1

    return occupied


def solve1(lines):
    max_i, max_j, empty, coords, ccoords = parse_input(lines)
    result = run(
        max_i=max_i,
        max_j=max_j,
        empty=empty,
        coords=coords,
        ccoords=ccoords,
        neighbors=gen_next_part1,
        max_neighbors=4,
    )
    print("Part 1:", result)


def solve2(lines):
    max_i, max_j, empty, coords, ccoords = parse_input(lines)
    result = run(
        max_i=max_i,
        max_j=max_j,
        empty=empty,
        coords=coords,
        ccoords=ccoords,
        neighbors=gen_next_part2,
        max_neighbors=5,
    )
    print("Part 2:", result)


def main():
    with open("./input1.txt") as inputs:
        lines = [line.strip() for line in inputs]
        for _ in range(10):
            solve1(lines)
            solve2(lines)

        t0 = time.time()
        solve1(lines)
        solve2(lines)
        t1 = time.time()
        print("Time:", t1 - t0)


if __name__ == "__main__":
    main()

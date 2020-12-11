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
                c = i * max_j + j
                empty[c] = 1
                coords.append((i, j))

    ccoords = array.array("I", [0] * len(coords))
    for n, (i, j) in enumerate(coords):
        ccoords[n] = i * max_j + j

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
    # t0 = time.time()
    precompneighbors = [[]] * (max_i * max_j)
    for (i, j), c in zip(coords, ccoords):
        precompneighbors[c] = neighbors(i=i, j=j, max_i=max_i, max_j=max_j, empty=empty)
    # t1 = time.time()
    # print('neighbors:', t1 - t0)

    # t0 = time.time()
    # p = sum(empty)

    check_empty = 1
    while True:
        if check_empty == 1:
            swaps = []
            for c in ccoords:
                if empty[c] == 1:
                    for new in precompneighbors[c]:
                        if empty[new] == 0:
                            break
                    else:
                        swaps.append(c)

            if not swaps:
                break

            check_empty = 0
        else:
            swaps = []
            for c in ccoords:
                if empty[c] == 0:
                    s = 0
                    for new in precompneighbors[c]:
                        if empty[new] == 0:
                            s += 1
                        if s >= max_neighbors:
                            swaps.append(c)
                            break
            check_empty = 1

        if not swaps:
            break
        for c in swaps:
            empty[c] = check_empty

        # if new_empty:
        #     p2 = len(new_empty)
        # elif new_taken:
        #     p2 = len(new_taken)
        # print(p - p2)
        # p = p2

        # if not (new_empty or new_taken):
        #     break

        # if new_empty and new_taken:
        #     print('????')
        # print('>>')
        # print('empty:', len(new_empty))
        # print('taken:', len(new_taken))
        # for c in new_empty:
        #     empty[c] = 1

        # for c in new_taken:
        #     empty[c] = 0

        # check_empty = 0 if check_empty == 1 else 1

    # t1 = time.time()
    # print('loops:', t1 - t0)
    return len(coords) - sum(empty)


def solve1(lines):
    # t0 = time.time()
    max_i, max_j, empty, coords, ccoords = parse_input(lines)
    # t1 = time.time()
    # print('read:', t1 - t0)
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
    with open('./input1.txt') as inputs:
        lines = [line.strip() for line in inputs]
        # print(len(lines))
        # print(len(lines[0]))
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

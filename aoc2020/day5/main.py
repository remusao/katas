import itertools


def get_seat_id(bp):
    r = 0
    for i, l in enumerate(bp[:7][::-1]):
        if l == "B":
            r += 2 ** i

    c = 0
    for i, l in enumerate(bp[7:][::-1]):
        if l == "R":
            c += 2 ** i

    return r * 8 + c


def main():
    with open("./input1.txt") as inputs:
        ids = [get_seat_id(line.strip()) for line in inputs]
        min_id = min(ids)
        max_id = max(ids)
        print("Part 1:", max_id)
        print("Part 2:", sum(range(min_id, max_id + 1)) - sum(ids))


if __name__ == "__main__":
    main()

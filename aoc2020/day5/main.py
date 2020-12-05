import itertools


def get_seat_id(bp):
    i = 1 << 6
    r = 0
    for l in bp[:7]:
        if l == "B":
            r += i
        i >>= 1

    i = 1 << 2
    c = 0
    for l in bp[7:]:
        if l == "R":
            c += i
        i >>= 1

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

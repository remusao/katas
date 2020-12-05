import itertools


def get_seat_id(bp):
    i = 1 << 6
    r = 128
    for l in bp[:7]:
        if l == "F":
            r -= i
        i >>= 1
    r -= 1

    i = 1 << 2
    c = 8
    for l in bp[7:]:
        if l == "L":
            c -= i
        i >>= 1
    c-= 1

    return r * 8 + c


def main():
    with open('./input1.txt') as inputs:
        ids = {get_seat_id(line.strip()) for line in inputs}
        min_id = min(ids)
        max_id = max(ids)
        print('Part 1:', max_id)
        print('Part 2:', set(range(min_id, max_id)) - ids)


if __name__ == "__main__":
    main()

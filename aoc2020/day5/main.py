def get_seat_id(bp):
    row = sum(2 ** i for i, l in enumerate(bp[:7][::-1]) if l == "B")
    col = sum(2 ** i for i, l in enumerate(bp[7:][::-1]) if l == "R")
    return row * 8 + col


def main():
    with open("./input1.txt") as inputs:
        ids = [get_seat_id(line.strip()) for line in inputs]
        min_id, max_id, sum_id = min(ids), max(ids), sum(ids)

        print("Part 1:", max_id)
        print("Part 2:", sum(range(min_id, max_id + 1)) - sum_id)


if __name__ == "__main__":
    main()

def main():
    with open("./input1.txt") as inputs:
        ids = [
            8 * sum(2 ** i for i, l in enumerate(line[0:7][::-1]) if l == "B")
            + sum(2 ** i for i, l in enumerate(line[7:10][::-1]) if l == "R")
            for line in inputs
        ]
        min_id, max_id, sum_id = min(ids), max(ids), sum(ids)

        print("Part 1:", max_id)
        print("Part 2:", sum(range(min_id, max_id + 1)) - sum_id)


if __name__ == "__main__":
    main()

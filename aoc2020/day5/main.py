def main():
    with open("./input1.txt") as inputs:
        ids = [
            int(line.translate(str.maketrans("BFRL", "1010")), 2)
            for line in inputs
        ]
        min_id, max_id, sum_id = min(ids), max(ids), sum(ids)
        print("Part 1:", max_id)
        print("Part 2:", sum(range(min_id, max_id + 1)) - sum_id)


if __name__ == "__main__":
    main()

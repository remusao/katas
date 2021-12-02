def solve1():
    with open("./input.txt") as inputs:
        pos, depth = 0, 0
        for line in inputs:
            action, value = line.strip().split(" ", 1)
            x = int(value)
            if action == "forward":
                pos += x
            elif action == "down":
                depth += x
            elif action == "up":
                depth -= x
        return pos * depth


def solve2():
    with open("./input.txt") as inputs:
        aim, pos, depth = 0, 0, 0
        for line in inputs:
            action, value = line.strip().split(" ", 1)
            x = int(value)
            if action == "forward":
                pos += x
                depth += aim * x
            elif action == "down":
                aim += x
            elif action == "up":
                aim -= x
        return pos * depth


if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

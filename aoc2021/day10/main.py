#!/usr/bin/env python

OPENING = {
    "]": "[",
    ")": "(",
    "}": "{",
    ">": "<",
}

def solve1():
    def impl(line):
        stack = []
        for c in line:
            if c in ("[", "(", "{", "<"):
                stack.append(c)
            elif c in ("]", ")", "}", ">"):
                if stack[-1] != OPENING[c]:
                    return c
                stack.pop(-1)
        return None


    with open("./input.txt") as inputs:
        score = 0
        for line in inputs:
            score += {
                ")": 3,
                "]": 57,
                "}": 1197,
                ">": 25137,
            }.get(impl(line.strip()), 0)
        return score

def solve2():
    def impl(line):
        stack = []
        for c in line:
            if c in ("[", "(", "{", "<"):
                stack.append(c)
            elif c in ("]", ")", "}", ">"):
                if stack[-1] != OPENING[c]:
                    return None

                stack.pop(-1)
        return stack


    with open("./input.txt") as inputs:
        scores = []
        for line in inputs:
            score = 0
            stack = impl(line.strip())
            if stack:
                for c in stack[::-1]:
                    score = (score * 5) + {
                        "(": 1,
                        "[": 2,
                        "{": 3,
                        "<": 4,
                    }.get(c, 0)
                scores.append(score)
        scores.sort()
        print(scores)
        return scores[int(len(scores) / 2)]




if __name__ == "__main__":
    print("Part 1:", solve1())
    print("Part 2:", solve2())

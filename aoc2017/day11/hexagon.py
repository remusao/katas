
from collections import Counter
import sys

simplification = {
    ('n', 's'): None,
    ('ne', 'sw'): None,
    ('nw', 'se'): None,
    ('ne', 's'): 'se',
    ('n', 'se'): 'ne',
    ('nw', 'ne'): 'n',
    ('s', 'nw'): 'sw',
    ('se', 'sw'): 's',
}

def solve1(instructions):
    instructions = Counter(instructions)
    while True:
        for (d1, d2), out in simplification.items():
            if instructions[d1] and instructions[d2]:
                instructions[d1] -= 1
                instructions[d2] -= 1
                if out is not None:
                    instructions[out] += 1
                    break
        else:
            break
    return sum(instructions.values())

def solve2(instructions):
    current = []
    max_distance = 0

    for instr in instructions:
        current.append(instr)
        max_distance = max(max_distance, solve1(current))

    return max_distance

def main():
    instr = sys.stdin.read().strip().split(',')
    print(solve2(instr))

if __name__ == '__main__':
    main()

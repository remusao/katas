

import sys


def score(program):
    s = 1
    total = 0
    for c in program:
        if c == 'C':
            s *= 2
        else:
            total += s
    return total


def solve(d, program):
    swaps = 0
    total = score(program)
    while total > d:
        # Try to find a swap
        for i in range(1, len(program)):
            if program[i - 1] == 'C' and program[i] == 'S':
                swaps += 1
                program[i - 1], program[i] = 'S', 'C'
                total = score(program)
                break
        else:
            # Not possible
            return None

    return swaps


def main():
    _, *testcases = sys.stdin.readlines()
    for i, testcase in enumerate(testcases):
        d, program = testcase.strip().split()
        swaps = solve(int(d), list(program))
        print('Case #{n}: {swaps}'.format(
            n=i + 1,
            swaps=swaps if swaps is not None else 'IMPOSSIBLE'
        ))


if __name__ == "__main__":
    main()

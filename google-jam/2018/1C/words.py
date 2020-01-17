

from collections import Counter
import sys



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

def total(letters, start=0):
    t = 1
    for j in range(start, len(letters)):
        t *= len(letters[j])
    return t


def main():
    t, *testcases = sys.stdin.readlines()
    t = int(t)
    testcases = iter(testcases)
    for i in range(1, t + 1):
        n, l = map(int, next(testcases).strip().split())
        words = set([
            tuple(next(testcases).strip())
            for _ in range(n)
        ])
        letters = []
        for j in range(l):
            letters.append(Counter(
                w[j] for w in words
            ))

        total_combinations = total(letters, 0)
        print(i, words)
        print(total_combinations, letters)
        result = '-'
        if total_combinations != len(words):
            # Find one word which is not present
            for j in range(l - 1):
                total_combinations = total(letters, j + 1)
                candidate = None
                for letter, count in letters[j].items():
                    if count < total_combinations:
                        candidate = letter
                        break
                print('>>', j, total_combinations)
                print(letters[j])
                print('CANDIDATE', candidate)
            result = 'MAYBE?'
        print('Case #{n}: {result}'.format(
            n=i,
            result=result
        ))


if __name__ == "__main__":
    main()

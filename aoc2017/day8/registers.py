

from collections import Counter
import sys


def main():
    registers = Counter()
    condition = {
        '==': lambda a, b: int(a) == int(b),
        '!=': lambda a, b: int(a) != int(b),
        '<': lambda a, b: int(a) < int(b),
        '<=': lambda a, b: int(a) <= int(b),
        '>': lambda a, b: int(a) > int(b),
        '>=': lambda a, b: int(a) >= int(b),
    }
    max_value = 0
    for line in sys.stdin.read().splitlines():
        r1, action, value, _, r2, op, v1 = line.split()
        if condition[op](registers[r2], v1):
            if action == 'inc':
                registers[r1] += int(value)
            elif action == 'dec':
                registers[r1] -= int(value)
            else:
                print('ERROR', line)
        max_value = max(registers[r1], max_value)

    if not registers:
        print(0)
    else:
        print(registers.most_common(1)[0][1])

    print('Max value', max_value)


if __name__ == '__main__':
    main()

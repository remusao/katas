
import sys
from collections import defaultdict


def solve1(instructions):
    registers = defaultdict(int)

    def get_value(x):
        try:
            return int(x)
        except:
            return registers[x]

    result = 0
    pos = 0
    while pos >= 0 and pos < len(instructions):
        instr, args = instructions[pos]
        if instr == 'set':
            registers[args[0]] = get_value(args[1])
        elif instr == 'sub':
            registers[args[0]] -= get_value(args[1])
        elif instr == 'mul':
            result += 1
            registers[args[0]] *= get_value(args[1])
        elif instr == 'jnz':
            if get_value(args[0]) != 0:
                pos += get_value(args[1])
                continue
        pos += 1
    return result

def transpile(instructions):
    print("""
int run() {
    int result = 0;
    int a = 0,
        b = 0,
        c = 0,
        d = 0,
        e = 0,
        f = 0,
        g = 0,
        h = 0;
    """)
    for i, (instr, args) in enumerate(instructions):
        output = f'    jump{i}:'
        if instr == 'set':
            print(f'{output} {args[0]} = {args[1]};')
        elif instr == 'sub':
            print(f'{output} {args[0]} -= {args[1]};')
        elif instr == 'mul':
            print(f'{output} {args[0]} *= {args[1]};')
            print('    result += 1;')
            # registers[args[0]] *= get_value(args[1])
        elif instr == 'jnz':
            print(f'{output} if ({args[0]} != 0) goto jump{max(0, i + int(args[1]))}; // {i}: f{instr} {args[0]} {args[1]}')
    print(f'    jump{len(instructions)} return result;')
    print('}')

def main():
    instructions = []
    for line in sys.stdin.read().splitlines():
        instr, *args = line.split()
        instructions.append((instr, args))
    transpile(instructions)


if __name__ == '__main__':
    main()

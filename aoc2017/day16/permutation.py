
import sys

def read_instructions():
    instructions = []
    for instr in sys.stdin.read().strip().split(','):
        if instr[0] == 's':
            instructions.append((0, int(instr[1:]) % 16))
        elif instr[0] == 'x':
            a, b = map(int, instr[1:].split('/'))
            instructions.append((1, (a, b)))
        elif instr[0] == 'p':
            a, b = instr[1:].split('/')
            instructions.append((2, (ord(a) - ord('a'), ord(b) - ord('a'))))
    return tuple(instructions)

def display(perm):
    print(''.join(chr(e + ord('a')) for e in perm))

def solve1(perm, instructions):
    for (instr, arg) in instructions:
        if instr == 0:
            perm = perm[16 - arg:] + perm[:16 - arg]
        elif instr == 1:
            perm[arg[0]], perm[arg[1]] = perm[arg[1]], perm[arg[0]]
        elif instr == 2:
            a, b = perm.index(arg[0]), perm.index(arg[1])
            perm[a], perm[b] = perm[b], perm[a]
    return perm

def solve2(perm, instructions, n=1000000000):
    original = list(perm)
    i = 0
    while True:
        i += 1
        perm = solve1(perm, instructions)
        if perm == original:
            for i in range(n % i):
                perm = solve1(perm, instructions)
            return perm


def main():
    perm = list(range(16))
    instructions = read_instructions()

    display(solve1(perm, instructions))
    display(solve2(perm, instructions))

if __name__ == '__main__':
    main()

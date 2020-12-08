
def parse(filename):
    with open('./input1.txt') as inputs:
        program = []
        for line in inputs:
            instr, arg = line.strip().split()
            program.append((instr, int(arg)))
        return program


def run(program):
    seen = set()
    acc = 0
    offset = 0
    while True:
        if offset in seen:
            break

        if offset >= len(program):
            break

        instr, arg = program[offset]
        seen.add(offset)

        if instr == 'acc':
            acc += arg
            offset += 1
        elif instr == 'jmp':
            offset += arg
        else:
            offset += 1

    return (offset == len(program), acc)


def solve1():
    _, acc = run(parse('./example.txt'))
    print('Part 1:', acc)


def solve2():
    program = parse('./example.txt')
    for i, (instr, arg) in enumerate(program):
        if instr == 'nop':
            program[i] = ('jmp', arg)
        elif instr == 'jmp':
            program[i] = ('nop', arg)

        success, acc = run(program)
        if success:
            print('Part 2:', acc)
            break

        program[i] = (instr, arg)


if __name__ == "__main__":
    solve1()
    solve2()


def solve1():
    with open('./input1.txt') as inputs:
        instrs = []
        for line in inputs:
            instr, arg = line.strip().split()
            instrs.append((instr, int(arg)))


        seen = set()
        acc = 0
        offset = 0
        while True:
            print('offset', offset)
            if offset in seen:
                print('res', acc)
                break

            instr, arg = instrs[offset]
            print(instr, arg)
            seen.add(offset)

            if instr == 'acc':
                acc += arg
                offset += 1
                print(acc)
            elif instr == 'jmp':
                offset += arg
            else:
                offset += 1


def run(instrs):
    seen = set()
    acc = 0
    offset = 0
    while True:
        print('offset', offset)
        if offset in seen:
            print('res', acc)
            break

        if offset >= len(instrs):
            break

        instr, arg = instrs[offset]
        print(instr, arg)
        seen.add(offset)

        if instr == 'acc':
            acc += arg
            offset += 1
            print(acc)
        elif instr == 'jmp':
            offset += arg
        else:
            offset += 1

    return (offset == len(instrs), acc)


def solve2():
    with open('./input1.txt') as inputs:
        instrs = []
        for line in inputs:
            instr, arg = line.strip().split()
            instrs.append((instr, int(arg)))

        for i, _ in enumerate(instrs):
            instr, arg = instrs[i]
            if instr == 'nop':
                instrs[i] = ('jmp', arg)
            elif instr == 'jmp':
                instrs[i] = ('nop', arg)

            success, acc = run(instrs)
            if success:
                print('>', acc)
                break
            instrs[i] = (instr, arg)






if __name__ == "__main__":
    solve2()

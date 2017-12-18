
from collections import defaultdict
import sys

import queue
import threading


def solve1(instructions):
    registers = defaultdict(int)
    last_sound = 0
    pos = 0

    def get_value(x):
        try:
            return int(x)
        except:
            return registers[x]

    while pos >= 0 and pos < len(instructions):
        instr, args = instructions[pos]
        if instr == 'snd':
            x = get_value(args[0])
            last_sound = x
        elif instr == 'set':
            registers[args[0]] = get_value(args[1])
        elif instr == 'add':
            registers[args[0]] += get_value(args[1])
        elif instr == 'mul':
            registers[args[0]] *= get_value(args[1])
        elif instr == 'mod':
            registers[args[0]] = registers[args[0]] % get_value(args[1])
        elif instr == 'rcv':
            if get_value(args[0]) != 0:
                return last_sound
        elif instr == 'jgz':
            if get_value(args[0]) > 0:
                y = get_value(args[1])
                pos += y
                continue
        else:
            raise 'Unknown instruction: ' + instr
        pos += 1

def run(instructions, pid, receive, send, result):
    registers = defaultdict(int)
    registers['p'] = pid
    pos = 0
    number_sent = 0

    def get_value(x):
        try:
            return int(x)
        except:
            return registers[x]

    while pos >= 0 and pos < len(instructions):
        instr, args = instructions[pos]
        if instr == 'snd':
            number_sent += 1
            x = get_value(args[0])
            send.put(x)
        elif instr == 'set':
            registers[args[0]] = get_value(args[1])
        elif instr == 'add':
            registers[args[0]] += get_value(args[1])
        elif instr == 'mul':
            registers[args[0]] *= get_value(args[1])
        elif instr == 'mod':
            registers[args[0]] = registers[args[0]] % get_value(args[1])
        elif instr == 'rcv':
            try:
                x = receive.get(timeout=0.1)
                registers[args[0]] = x
            except queue.Empty:
                break
        elif instr == 'jgz':
            if get_value(args[0]) > 0:
                y = get_value(args[1])
                pos += y
                continue
        else:
            raise 'Unknown instruction: ' + instr
        pos += 1
    if pid == 1:
        result.put(number_sent)

def solve2(instructions):
    q0to1 = queue.Queue()
    q1to0 = queue.Queue()
    result = queue.Queue()

    p0 = threading.Thread(
        target=run,
        kwargs={
            'instructions': instructions,
            'pid': 0,
            'receive': q1to0,
            'result': None,
            'send': q0to1,
        },
    )
    p1 = threading.Thread(
        target=run,
        kwargs={
            'instructions': instructions,
            'pid': 1,
            'receive': q0to1,
            'result': result,
            'send': q1to0,
        },
    )

    # Start running the programs
    p0.start()
    p1.start()

    # Wait
    number_sent = result.get()
    p0.join()
    p1.join()
    return number_sent

def main():
    instructions = []
    for line in sys.stdin.read().splitlines():
        instr, *args = line.split()
        instructions.append((instr, args))

    print('Part1', solve1(instructions))
    print('Part2', solve2(instructions))

if __name__ == '__main__':
    main()

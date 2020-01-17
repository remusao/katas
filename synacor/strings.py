

with open('challenge.bin', 'rb') as input_program:
    program = input_program.read()
    chars = []
    pos = 0
    while pos < len(program):
        b0, b1 = program[pos], program[pos + 1]
        value = b0 | (b1 << 8)
        if value <= 1000:
            print(chr(value), end='')
        pos += 2

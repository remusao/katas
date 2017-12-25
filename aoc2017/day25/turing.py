
import sys

def read_action(lines, pos):
    return (
        int(lines[pos].rsplit(' ', 1)[-1][:-1]),
        1 if lines[pos + 1].rsplit(' ', 1)[-1][:-1] == 'right' else -1,
        lines[pos + 2].rsplit(' ', 1)[-1][:-1],
    )

def read_blueprint():
    start, steps, *lines = sys.stdin.read().strip().splitlines()
    start_state = start.rsplit(' ', 1)[-1][:-1]
    number_steps = int(steps.rsplit(' ', 2)[-2])
    blueprint = {}
    pos = 0
    while pos < len(lines):
        line = lines[pos]
        if line.startswith('In state'):
            blueprint[line.rsplit(' ', 1)[-1][:-1]] = {
                False: read_action(lines, pos + 2),
                True: read_action(lines, pos + 6),
            }
            pos += 9
        else:
            pos += 1
    return start_state, number_steps, blueprint

def main():
    state, steps, blueprint = read_blueprint()
    tap = set()
    pos = 0
    for _ in range(steps):
        new_value, direction, state = blueprint[state][pos in tap]
        if new_value == 0 and pos in tap:
            tap.remove(pos)
        elif new_value == 1:
            tap.add(pos)
        pos += direction
    print(len(tap))


if __name__ == '__main__':
    main()

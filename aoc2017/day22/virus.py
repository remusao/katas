import sys

def read_grid():
    lines = sys.stdin.read().strip().splitlines()
    grid = {}
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            if c == '#':
                grid[x + y * 1j] = '#'
    return grid, (len(lines[0]) // 2) + (len(lines) // 2) * 1j

def print_grid(grid, current):
    minx, maxx = int(min(c.real for c in grid)), int(max(c.real for c in grid))
    miny, maxy = int(min(c.imag for c in grid)), int(max(c.imag for c in grid))

    print()
    print('>>', current)
    for y in range(miny - 1, maxy + 2):
        for x in range(minx - 1, maxx + 2):
            p = x + y * 1j
            if p == current:
                print('[', end='')
            else:
                print(' ', end='')
            print(grid.get(x + y * 1j, '.'), end='')
            if p == current:
                print(']', end='')
            else:
                print(' ', end='')
        print()

LEFT = { -1j: -1, -1: 1j, 1j: 1, 1: -1j }
RIGHT = { -1j: 1, 1: 1j, 1j: -1, -1: -1j }

def run(grid, start, n, change_direction, change_state):
    current = start
    direction = -1j
    result = 0

    for _ in range(n):
        state = grid.get(current, '.')
        direction = change_direction(direction, state)
        new_state = change_state(state)

        if new_state == '.':
            del grid[current]
        else:
            grid[current] = new_state

        if new_state == '#':
            result += 1

        current += direction

    return result

def solve1(grid, start):
    def change_direction(direction, state):
        if state == '.':
            return LEFT[direction]
        return RIGHT[direction]

    def change_state(state):
        if state == '.':
            return '#'
        return '.'

    return run(grid, start, 10000, change_direction, change_state)

def solve2(grid, start):
    def change_direction(direction, state):
        if state == '.':
            return LEFT[direction]
        elif state == 'W':
            return direction
        elif state == '#':
            return RIGHT[direction]
        else:
            return -1 * direction

    def change_state(state):
        if state == '.':
            return 'W'
        elif state == 'W':
            return '#'
        elif state == '#':
            return 'F'
        else:
            return '.'

    return run(grid, start, 10000000, change_direction, change_state)

def main():
    grid, start = read_grid()
    print(solve1(grid.copy(), start))
    print(solve2(grid.copy(), start))

if __name__ == '__main__':
    main()

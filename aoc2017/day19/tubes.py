
import sys

def read_grid():
    grid = {}
    start = None
    for y, line in enumerate(sys.stdin.read().splitlines()):
        for x, c in enumerate(line):
            if c.strip() != '':
                pos = x + y * 1j
                if start is None and c == '|':
                    start = pos
                grid[pos] = c
    return grid, start

def route(grid, current):
    direction = 1j
    yield current, grid[current]
    while (current + direction) in grid:
        current += direction
        c = grid[current]
        yield (current, c)
        if c == '+':
            for d in [1, -1, 1j, -1j]:
                if d != -direction and (current + d) in grid:
                    direction = d
                    break

def solve1(grid, start):
    return ''.join(c for (_, c) in route(grid, start) if c not in '+|-')

def solve2(grid, start):
    return len(list(route(grid, start)))

def main():
    grid, start = read_grid()
    print(solve1(grid, start))
    print(solve2(grid, start))

if __name__ == '__main__':
    main()

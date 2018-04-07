
from collections import defaultdict
import math
import sys

# def solve(a, b):
#     m = (a + b) // 2
#     print(m)
#     sys.stdout.flush()
#     s = input()
#
#     if s == "CORRECT":
#         return
#     elif s == "TOO_SMALL":
#         a = m + 1
#     else:
#         b = m - 1
#
#     solve(a, b)


def debug(*args):
    print(*args, file=sys.stderr, flush=True)


def output(*args):
    print(*args, flush=True)


def main():
    deltas = [
        - 1,        # top
        + 1,        # bottom
        - 1j,       # left
        + 1j,       # right
        + 1 - 1j,   # bottom-left
        + 1 + 1j,   # bottom-right
        - 1 + 1j,   # top-right
        - 1 - 1j,   # top-left
    ]

    t = int(input())

    # debug('T', t)

    for n in range(1, t + 1):
        # Number of cells we need to cover
        a = int(input())

        # debug('A', a)

        # Find a size of rectangle we want to cover
        sqrt = int(math.sqrt(a))
        rows, cols = sqrt, sqrt

        if rows < 3:
            rows = 3
        if cols < 3:
            cols = 3
        if rows * cols < a:
            cols += 1

        # debug('rows', rows, 'cols', cols, 'total', rows * cols)

        # Collect all empty cells
        positions = {
            i + j * 1j
            for i in range(1, rows + 1)
            for j in range(1, cols + 1)
        }

        # Collect all positions
        densities = defaultdict(set)
        densities[0] = {
            i + j * 1j
            for i in range(2, rows)
            for j in range(2, cols)
        }

        cell2density = {
            cell: density
            for (density, cells) in densities.items()
            for cell in cells
        }

        # debug('positions', len(positions))
        # debug('cell2density', cell2density)
        # debug('densities', densities)

        # debug('positions', positions)
        # for i in range(1, 20):
        #     for j in range(1, 20):
        #         if i + j * 1j in positions:
        #             sys.stdout.write('#')
        #         else:
        #             sys.stdout.write('.')
        #     print(flush=True)

        while positions:
            # Find best next position
            best_pos = 0
            for i in range(9):
                density = densities[i]
                if density:
                    best_pos = next(iter(density))
                    # density.remove(best_pos)
                    break

            # Send position to judge
            output(int(best_pos.real), int(best_pos.imag))

            # Handle response from Judge
            i_res, j_res = map(int, input().strip().split())
            pos = i_res + j_res * 1j

            if (i_res, j_res) == (-1, -1):
                # ERROR
                return
            elif (i_res, j_res) == (0, 0):
                # SUCCESS
                break
            elif pos in positions:
                positions.remove(pos)

                for delta in deltas:
                    cell = pos + delta
                    if cell in cell2density:
                        density = cell2density[cell]
                        densities[density].remove(cell)
                        densities[density + 1].add(cell)
                        cell2density[cell] = density + 1

            # debug('positions', len(positions))


if __name__ == "__main__":
    main()

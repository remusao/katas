

def count_left(grid, r):
    # real = row
    return sum(
        1 for c in grid
        if c.real < r
    )

def count_right(grid, r):
    # real = row
    return sum(
        1 for c in grid
        if c.real >= r
    )

def print_grid(grid, r, c):
    for i in range(r):
        for j in range(c):
            if (j + i * 1j) in grid:
                print('@', end='')
            else:
                print('.', end='')
        print()


def main():
    t = int(input())
    for n in range(1, t + 1):
        r, c, h, v = map(int, input().strip().split())
        grid = set()
        for i in range(r):
            for j, value in enumerate(input().strip()):
                if value == '@':
                    grid.add(j + i * 1j)

        number_of_chips = len(grid)
        number_of_slices = (h + 1) * (v + 1)
        if number_of_chips % number_of_slices != 0:
            print(n, 'IMPOSSIBLE')
        else:
            chips_per_slice = number_of_chips // number_of_slices
            if chips_per_slice == 0:
                print(n, 'POSSIBLE')
            else:
                print(
                    n,
                    'number of choc chips:', number_of_chips,
                    'number of slices:', number_of_slices,
                    'choc chips per slice', chips_per_slice,
                )

                print_grid(grid, r, c)

                possible_v_cuts = []
                for x in range(1, c):
                    left = count_left(grid, x)
                    right = number_of_chips - left
                    if left % chips_per_slice == 0 and right % chips_per_slice == 0:
                        possible_v_cuts.append(x)

                print('>>>', possible_v_cuts)

                print()

if __name__ == "__main__":
    main()


import math
import sys

INF = 2**64

def iter_adjacents(x, y, vertices):
    adjacents = [
        (x + 1, y),
        (x, y + 1),
        (x - 1, y),
        (x, y - 1),
    ]
    for adj in adjacents:
        if adj in vertices:
            yield adj, vertices[adj]

def solve(matrix):
    # Initialization
    spt = {}
    distances = {}
    for (x, y), value in matrix.items():
        distances[(x, y)] = INF

    # Start from bottom-right corner
    size = int(math.sqrt(len(matrix))) - 1
    max_x, max_y = size, size
    distances[(max_x, max_y)] = matrix[(max_x, max_y)]

    # Iterate
    while distances:
        (x, y), value = min(distances.items(), key=lambda k: k[1])

        if (x, y) == (0, 0):
            return value

        del distances[(x, y)]
        spt[(x, y)] = value
        for (ax, ay), av in iter_adjacents(x, y, distances):
            distances[(ax, ay)] = min(
                value + matrix[(ax, ay)],
                av
            )

    return spt[(0, 0)]

def read_matrix():
    matrix = {}
    for y, line in enumerate(sys.stdin.read().splitlines()):
        for x, value in enumerate(line.split(',')):
            matrix[(x, y)] = int(value)
    return matrix

def main():
    print(solve(read_matrix()))


if __name__ == '__main__':
    main()

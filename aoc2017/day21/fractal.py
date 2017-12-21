
import numpy
import sys

def s2a(flat):
    return numpy.array([
        [1 if c == '#' else 0 for c in line]
        for line in flat.split('/')
    ])

def a2s(array):
    return '/'.join([
        ''.join('#' if c == 1 else '.' for c in line)
        for line in array
    ])

def replace(array, size, n, book):
    return numpy.concatenate([
        numpy.concatenate([
            book[a2s(array[(i * n):(i * n + n), (j * n):(j * n + n)])]
            for j in range(size // n)
        ], 1)
        for i in range(size // n)
    ])

def read_book():
    book = {}
    for line in sys.stdin.read().strip().splitlines():
        source, result = map(s2a, line.split(' => '))
        for _ in range(4):
            book[a2s(source)] = result
            book[a2s(numpy.flipud(source))] = result
            source = numpy.rot90(source)
    return book

def main():
    book = read_book()
    current = s2a('.#./..#/###')
    for it in range(18):
        size = current.shape[0]
        if size % 2 == 0:
            current = replace(current, size, 2, book)
        else:
            current = replace(current, size, 3, book)
    print(a2s(current).count('#'))

if __name__ == '__main__':
    main()

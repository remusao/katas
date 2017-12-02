
from __future__ import print_function
from pyprimes import erat


def main():
    limit = 50000000
    primes = erat(7000)
    squares = map(lambda p: p * p, primes)
    cubes = map(lambda p: p * p * p, primes)
    fourth = map(lambda p: p * p, squares)

    candidates = set()
    for sq in squares:
        for cu in cubes:
            for fo in fourth:
                if (sq + cu + fo) < limit:
                    candidates.add(sq + cu + fo)
    print(len(candidates))


main()

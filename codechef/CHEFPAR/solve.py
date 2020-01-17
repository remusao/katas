
from itertools import cycle
from operator import mul
from functools import reduce


def ints():
    return list(map(int, input().strip().split()))


def score(Dx, Ax, Px):
    print('Dx', Dx)
    print('Ax', Ax)
    print('Px', Px)
    product = reduce(mul, [a + d for (d, a) in zip(Dx, Ax)])
    score = 0.0
    for p in Px:
        score += product % p
    return score / len(Px)


def main():
    _, _, k = ints()
    Ax = ints()
    Px = ints()
    Dx = []
    for a, p in zip(Ax, cycle(Px[::-1])):
        pass
    print(score([1, 4, 3, 5], Ax, Px))



if __name__ == "__main__":
    main()


from itertools import islice
import math

def continued(x):
    """https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion."""
    m = 0
    d = 1
    a0 = int(math.floor(math.sqrt(x)))
    a = a0

    yield a

    while True:
        m = d * a - m
        d = (x - m * m) // d
        a = (a0 + m) // d
        yield a

def detect_period(xs):
    s = ''.join(map(chr, islice(xs, 500)))[1:]
    for i in range(1, 300):
        prefix = s[:i]
        if s.startswith(prefix, i):
            pos = i + i
            ok = True
            while (pos + i) < len(s):
                if not s.startswith(prefix, pos):
                    ok = False
                    break
                pos += i
            if ok:
                return i

def is_square(n):
    r = int(math.sqrt(n))
    return r * r == n

def main():
    total = 0
    for n in range(10001):
        if not is_square(n):
            period_length = detect_period(continued(n))
            if period_length % 2 != 0:
                total += 1
            print(n, period_length)
    print('RESULT', total)

if __name__ == '__main__':
    main()

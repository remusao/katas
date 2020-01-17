

from decimal import *
from itertools import count, islice

getcontext().prec = 2000

def omit(s):
    d = s % 10
    s = s // 10
    n = 1
    while s > 0:
        if d != s % 10:
            d = s % 10
            n = 1
        else:
            n += 1
        if n >= 3:
            return True
        s = s // 10
    return False

def denominators():
    for s in count(1):
        if not omit(s):
            yield s

def main():
    ten = Decimal(10)
    print(ten * ten.ln())
    return
    result = Decimal(0)
    one = Decimal(1)
    for i, d in enumerate(denominators()):
        result += one / Decimal(d)
        if i % 100000 == 0:
            print(result)


if __name__ == '__main__':
    main()

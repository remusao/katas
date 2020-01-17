#!/usr/bin/env python
# -*- coding: utf-8 -*-


import sys
from collections import Counter
from decimal import *
getcontext().prec = 500

def debug(*args):
    if True:
        print(*args)

def split(d):
    integer_part = d.quantize(Decimal('1.'), rounding=ROUND_DOWN)
    decimal_part = d - integer_part
    return integer_part, decimal_part

def main():
    t, *lines= sys.stdin.readlines()
    t = int(t)
    for i in range(t):
        n, l = map(int, lines[2 * i].strip().split())
        progs = Counter()
        for j, v in enumerate(map(int, lines[2 * i + 1].strip().split())):
            progs[j] = v

        ratio = Decimal(100) / Decimal(n)
        integer_part, decimal_part = split(ratio)

        debug('RATIO', ratio)
        debug('INT', integer_part)
        debug('DECIMAL', decimal_part)

        good = set()
        for j in range(1, n + 1):
            m = decimal_part * Decimal(j)
            ipart, dpart = split(m)
            if dpart >= Decimal(0.5):
                good.add(j)
            debug('>>>', j, dpart >= Decimal(0.5), m)

        bad2good = {}
        last = -1
        for j in range(1, n + 1):
            if j in good:
                for k in range(j - 1, last, -1):
                    if k not in good:
                        bad2good[k] = j - k
                last = j

        bad2good[n + 1] = n
        for j in range(n, 0, -1):
            if j not in bad2good:
                if (j + 1) in good:
                    bad2good[j] = 1 + n
                else:
                    bad2good[j] = 1 + bad2good[j + 1]
        bad2good[0] = bad2good[1] + 1

        debug('BAD2GOOD', bad2good)
        remaining = n - sum(progs.values())
        debug('REMAIN', remaining)

        def score(v):
            p, value = v
            if value in good:
                return n + 1
            return bad2good[value]

        while remaining > 0:
            p, value = min(progs.items(), key=score)

            to_add = 0

            debug('FOUND', p, value)
            if bad2good[0] < bad2good[value]:
                # It's more interesting to answer a new program
                to_add = bad2good[0]
                p = len(progs)
            else:
                if value in good:
                    to_add = bad2good[value] - n
                else:
                    to_add = bad2good[value]

            if to_add > remaining:
                progs[0] += remaining
                break
            else:
                progs[p] += to_add
                remaining -= to_add

        debug(progs)
        result = 0
        for v in progs.values():
            result += ratio * Decimal(v)
        debug(n, l, progs)
        print('Case #{n}: {result}'.format(
            n=i + 1,
            result=result
        ))


if __name__ == "__main__":
    main()

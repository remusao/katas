#!/usr/bin/env python
# -*- coding: utf-8 -*-


import sys


def main():
    _, *testcases = sys.stdin.readlines()
    for i, testcase in enumerate(testcases):
        d, program = testcase.strip().split()
        swaps = solve(int(d), list(program))
        print('Case #{n}: {swaps}'.format(
            n=i + 1,
            swaps=swaps if swaps is not None else 'IMPOSSIBLE'
        ))


if __name__ == "__main__":
    main()

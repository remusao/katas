#! /usr/bin/env python
# -*- coding: utf-8 -*-


"""
Solve Google HashCode 2015.

Usage:
    solve [options] <in> <out>
    solve -h | --help

Options:
    -h, --help      Show help.
"""


from __future__ import unicode_literals, print_function
import docopt
from io import read_data, format_solution
from solve import solve
from optimize import optimize


def main():
    args = docopt.docopt(__doc__)
    data = read_data(args['<in>'])
    score, result = solve(data)
    print(score)
    score2, result2 = optimize(result, data)
    print(score2)
    format_solution(args['<out>'], result2, prefix=str(score))


if __name__ == "__main__":
    # # --###----#
    # l = Row(0, 10, [2, 3, 4, 9])
    # pprint(l.blocks)
    # # [2, 4]
    # print(l.freeslots())
    # print(l.getmax())
    # print(l.getmin())
    # l.useslot(3)
    # pprint(l.blocks)
    main()

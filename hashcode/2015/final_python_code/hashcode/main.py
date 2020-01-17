#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
42

Usage:
    hashcode_final [options] <pizza.txt>
    hashcode_final -h | --help

Options:
    -h, --help  Show help.
"""

from __future__ import print_function, unicode_literals
import docopt
from hc_io import read_file, dump_solution
from solve import solve


def main():
    """ Main function of project hashcode_final """
    args = read_file("final_round.in")
    for score, solution in solve(args):
        dump_solution(solution, str(score))


if __name__ == "__main__":
    main()

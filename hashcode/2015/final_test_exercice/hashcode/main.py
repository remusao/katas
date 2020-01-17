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
import hashcode.io as io
import hashcode.solve as solve


def main():
    """ Main function of project hashcode_final """
    args = docopt.docopt(__doc__)
    (R, C, H, S, pizza) = io.read_file(args["<pizza.txt>"])
    parts = solve.solve(pizza, H, S)
    io.dump_solution(parts)


if __name__ == "__main__":
    main()

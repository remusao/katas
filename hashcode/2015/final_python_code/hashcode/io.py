
# -*- coding: utf-8 -*-


from __future__ import print_function
import numpy as np

def read_file2(path):
    with open(path, "rb") as input_file:
        n_rows, n_cols, A = map(int, next(input_file).split())
        n_targets, radius, n_bals, n_rounds = map(int, next(input_file).split())
        r_init, c_init = map(int, next(input_file).split())
    pass


def dump_solution(parts, output_filename="output.sol"):
    pass

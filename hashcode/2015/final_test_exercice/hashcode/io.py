
# -*- coding: utf-8 -*-


from __future__ import print_function
import numpy as np

def read_file(path):
    lines = open(path).readlines()
    R, C, H, S = map(int, lines[0].strip().split(' '))
    arr = np.loadtxt(path, usecols=range(0), skiprows=1, dtype=str)
    mat = np.zeros((R, C))
    for r in range(arr.size):
        for c in range(len(arr[r])):
            if arr[r][c] == 'H':
                mat[r, c] = 1
            else:
                mat[r, c] = 0
    return (R, C, H, S, mat)


def dump_solution(parts, output_filename="output.sol"):
    with open(output_filename, 'w') as f:
        print(len(parts), file=f)
        for t in parts:
            print(' '.join(map(str, t)), file=f)

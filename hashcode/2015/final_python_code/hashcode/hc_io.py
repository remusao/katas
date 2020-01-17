
# -*- coding: utf-8 -*-


from __future__ import print_function
import numpy as np
from itertools import izip

def grouped(iterable, n):
    "s -> (s0,s1,s2,...sn-1), (sn,sn+1,sn+2,...s2n-1), (s2n,s2n+1,s2n+2,...s3n-1), ..."
    return izip(*[iter(iterable)]*n)


def SplitLine(line):
    return map(int, line.strip().split(' '))


def read_file(path):
    print("Read file")
    lines = open(path).readlines()
    R, C, A = map(int, lines[0].strip().split(' '))
    L, V, B, T = map(int, lines[1].strip().split(' '))
    Rs, Cs = map(int, lines[2].strip().split(' '))

    # Load targets.
    targets = np.zeros((L, 2))
    for i in range(3, 3 + L):
        Ri, Ci = map(int, lines[i].strip().split(' '))
        targets[i-3,:] = (Ri, Ci)

    # A sections.
    layers = []
    for section_id in range(0, A):
        forces = np.zeros((R, C, 2))
        for i in range(0, R):
            values = SplitLine(lines[3 + L + section_id * R + i])
            j = 0
            for r, c in grouped(values, 2):
                forces[i, j, :] = (r, c)
                j += 1
        layers.append(forces)

    print("Success reading file")
    return R, C, A, L, V, B, T, Rs, Cs, targets, layers


def dump_solution(result, output_filename="output.sol", prefix=""):
    with open(prefix + output_filename, 'wb') as output:
        for moves in result:
            print(' '.join(map(str, moves)), file=output)

#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys


def solve(inputs, index=0):
    number_of_children = inputs[index]
    number_of_metadata = inputs[index + 1]
    index += 2

    result = 0
    for _ in range(number_of_children):
        (sub_result, index) = solve(inputs=inputs, index=index)
        result += sub_result

    for i in range(number_of_metadata):
        result += inputs[index + i]

    return (result, index + number_of_metadata)


if __name__ == "__main__":
    print("Part 1:", solve(list(map(int, sys.stdin.read().strip().split())))[0])

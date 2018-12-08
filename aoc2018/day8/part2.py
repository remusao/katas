#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys


def solve(inputs, index=0):
    number_of_children = inputs[index]
    number_of_metadata = inputs[index + 1]
    index += 2

    # Get children of node
    children = {}
    for child_index in range(1, number_of_children + 1):
        (sub_result, index) = solve(inputs=inputs, index=index)
        children[child_index] = sub_result

    # Get metadata
    metadata = [inputs[index + i] for i in range(number_of_metadata)]

    # Compute value for this level
    return (
        sum(metadata)
        if number_of_children == 0
        else sum(children.get(child, 0) for child in metadata),
        index + number_of_metadata,
    )


if __name__ == "__main__":
    print("Part 2:", solve(list(map(int, sys.stdin.read().strip().split())))[0])

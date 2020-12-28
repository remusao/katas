#!/usr/bin/env python

import array
import time


def llcreate(arr, number_of_cups):
    nexts = array.array("I", [0] * (number_of_cups + 1))
    head = arr[0]
    prev = head
    for cup in arr[1:]:
        nexts[prev] = cup
        prev = cup

    if number_of_cups > len(arr):
        nexts[prev] = len(arr) + 1
        for cup in range(len(arr) + 1, number_of_cups):
            nexts[cup] = cup + 1
        prev = number_of_cups

    nexts[prev] = head

    return head, nexts


def run(cups, number_of_cups, n):
    current, nexts = llcreate(cups, number_of_cups)
    maxi = len(nexts) - 1

    for _ in range(n):
        next1 = nexts[current]
        next2 = nexts[next1]
        next3 = nexts[next2]

        destination = current
        while (
            destination == next1
            or destination == next2
            or destination == next3
            or destination == current
        ):
            destination = destination - 1 if destination != 1 else maxi

        nexts[current], nexts[next3], nexts[destination] = (
            nexts[next3],
            nexts[destination],
            next1,
        )

        current = nexts[current]

    return nexts


def solve1(cups):
    nexts = run(cups=cups, number_of_cups=len(cups), n=100)

    result = chr(nexts[1] + 48)
    current = nexts[nexts[1]]
    while current != 1:
        result += chr(current + 48)
        current = nexts[current]
    return result


def solve2(cups):
    nexts = run(cups=cups, number_of_cups=1000000, n=10000000)
    n1 = nexts[1]
    return n1 * nexts[n1]


def main():
    example = [3, 8, 9, 1, 2, 5, 4, 6, 7]
    input1 = [3, 9, 4, 6, 1, 8, 5, 2, 7]

    t0 = time.time()
    print("Part 1:", solve1(input1))
    print("Part 2:", solve2(input1))
    t1 = time.time()
    print("Pre-warmup:", t1 - t0)

    # Warm-up
    for _ in range(10):
        solve1(input1)
        solve2(input1)

    t0 = time.time()
    solve1(input1)
    solve2(input1)
    t1 = time.time()
    print("Time:", t1 - t0)


if __name__ == "__main__":
    main()

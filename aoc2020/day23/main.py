#!/usr/bin/env python

from collections.abc import MutableSequence


class Node(object):
    __slots__ = "value", "next_node"

    def __init__(self, value, next_node):
        self.value = value
        self.next_node = next_node


def lllast(head):
    last = head
    for node in lliter(head):
        last = node
    return last


def lliter(head):
    seen = set()
    while head not in seen:
        yield head
        seen.add(head)
        head = head.next_node


def lllist(head):
    return [node.value for node in lliter(head)]


def lltuple(head):
    return tuple(node.value for node in lliter(head))


def llprint(head):
    print("llist::", llrepr(head))


def llrepr(head):
    return repr(lllist(head))


def llcreate(arr):
    # Create all nodes at once
    memo = [Node(cup, None) for cup in range(len(arr) + 1)]

    # Create links
    head = memo[arr[0]]
    prev = head
    for value in arr[1:]:
        node = memo[value]
        prev.next_node = node
        prev = node
    prev.next_node = head
    return head, memo


def llslice(head, n):
    nextn = head.next_node
    current = nextn
    for _ in range(n - 1):
        current = current.next_node
    head.next_node = current.next_node
    current.next_node = nextn  # Make circular
    return nextn


def run(cups, n):
    # Create circular linked list
    current, memo = llcreate(cups)
    maxi = len(cups)

    for _ in range(n):
        # Remove three next after `current`
        next3 = llslice(current, 3)
        tnext3 = lltuple(next3)

        # Find destination node
        destination = current.value - 1
        if destination == 0:
            destination = maxi

        while destination in tnext3:
            destination -= 1
            if destination == 0:
                destination = maxi

        destination_node = memo[destination]

        # Insert 3 removed elements back
        lllast(next3).next_node = destination_node.next_node
        destination_node.next_node = next3
        current = current.next_node

    return memo[1]


def solve1(cups):
    return "".join([str(cup) for cup in lllist(run(cups, 100))[1:]])


def solve2(cups):
    head1 = run(cups + list(range(10, 1000001)), 10000000)
    n1 = head1.next_node.value
    n2 = head1.next_node.next_node.value
    return n1 * n2


if __name__ == "__main__":
    example = [3, 8, 9, 1, 2, 5, 4, 6, 7]
    input1 = [3, 9, 4, 6, 1, 8, 5, 2, 7]
    print("Part 1:", solve1(input1))
    print("Part 2:", solve2(input1))

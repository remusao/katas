

from __future__ import unicode_literals, print_function
import sys


def can_win(X, R, C):
    # Width of the N-omino
    width = min(R, C)
    # Number of cell taken
    area = 2 * width - 1
    # Number of cell free
    free = X - area
    # Dimension to recurse
    height = max(R, C)
    free_slots_mod = []
    free_slots = []
    for i in range(width, height):
        # Free slots ahead
        free_slot = (height - i) * width + (width - 1) * (width - 1)
        free_slots.append(free_slot)
    # Check those where Gabriel can win
    free_slots_mod = [(f % X != 0 or f < X) for f in free_slots if f > 0]
    taken = 0
    # Try to add one block at a time and see if Gabriel can still win
    while (free > 0) and (False in free_slots_mod):
        for i in range(height - width):
            free_slots[i] -= 1
        free_slots_mod = [(f % X != 0 or f < X) for f in free_slots if f > 0]
        free -= 1
        taken += 1

    # No need to check for `offset` first lines if we
    # filled those cells
    offset = 0
    if free % width:
        offset = 1
    if free >= width:
        offset += free / width

    # No need to check for `lastoffset` last lines if we
    # filled those cells
    lastoffset = len(free_slots_mod)
    spill = (taken - (width - 1) * (width - 1))
    if spill > 0:
        if spill % width:
            lastoffset -= 1
        if spill >= width:
            lastoffset -= spill / width

    free_slots_mod = free_slots_mod[offset:lastoffset]
    return (len(free_slots_mod) > 0) and (False not in free_slots_mod)


def main():
    with open(sys.argv[1], "rb") as input_file:
        T = int(next(input_file).strip())
        for t in range(1, T + 1):
            X, R, C = map(int, next(input_file).split())
            richard = False
            if (R * C) % X != 0:
                richard = True
            elif X >= 7:
                richard = True
            elif X > max(R, C):
                richard = True
            elif X >= (2 * (min(R, C) + 1) - 1):
                richard = True
            elif X > (2 * (min(R, C)) - 1):
                richard = can_win(X, R, C)
            print("Case #{t}: {winner}".format(
                t=t,
                winner="RICHARD" if richard else "GABRIEL"))



if __name__ == "__main__":
    main()

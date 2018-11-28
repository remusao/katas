#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys


def read_zombies_coordinates():
    grid = sys.stdin.read().strip()
    print("grid:")
    print(grid)
    return {
        x + y * 1j
        for y, line in enumerate(grid.splitlines())
        for x, c in enumerate(filter(lambda c: c == "0" or c == "1", line))
        if c == "1"
    }


def main():
    zombies_coordinates = read_zombies_coordinates()
    number_of_clusters = 0
    while zombies_coordinates:
        number_of_clusters += 1

        # Remove zombies from next cluster iteratively
        zombies_in_cluster = [zombies_coordinates.pop()]
        while zombies_in_cluster:
            zombie_coordinates = zombies_in_cluster.pop()
            neighbors = zombies_coordinates & {
                zombie_coordinates - 1,  # left
                zombie_coordinates + 1,  # right
                zombie_coordinates - 1j,  # top
                zombie_coordinates + 1j,  # bottom
            }
            zombies_in_cluster.extend(neighbors)
            zombies_coordinates -= neighbors

    print(f"Found {number_of_clusters} clusters")


if __name__ == "__main__":
    main()

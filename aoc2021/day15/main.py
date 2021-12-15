#!/usr/bin/env python

import sys
from queue import PriorityQueue


def pprint(grid):
    max_y = int(max(coord.imag for coord in grid))
    max_x = int(max(coord.real for coord in grid))
    for y in range(max_y + 1):
        for x in range(max_x + 1):
            print(grid[x + y * 1j], end="")
        print()


def dijkstra(grid):
    unvisited_set = set(grid)
    queue = []
    tentative_distance = {}
    for coord in grid:
        score = [sys.maxsize, coord]
        queue.append(score)
        tentative_distance[coord] = score
    qlen = len(queue) - 1
    tentative_distance[0][0] = 0

    max_y = int(max(coord.imag for coord in grid))
    max_x = int(max(coord.real for coord in grid))
    target = max_x + max_y * 1j

    queue.sort(key=lambda entry: entry[0], reverse=True)
    current_cost, current = queue[qlen]
    qlen -= 1
    while target in unvisited_set:
        print(len(unvisited_set))
        for delta in (-1, 1, 1j, -1j):
            neighbor = current + delta
            if neighbor in tentative_distance:
                cost = tentative_distance[neighbor][0]
                new_cost = current_cost + grid[neighbor]
                if new_cost < cost:
                    tentative_distance[neighbor][0] = new_cost
                    index = queue.index(tentative_distance[neighbor])
                    while index < qlen and queue[index][0] < queue[index + 1][0]:
                        queue[index], queue[index + 1] = queue[index + 1], queue[index]
                        index += 1

        unvisited_set.remove(current)
        if not unvisited_set:
            break

        current_cost, current = queue[qlen]
        qlen -= 1

    print(tentative_distance[target])


def solve1():
    with open("./input.txt") as inputs:
        grid = {}
        for y, line in enumerate(inputs):
            for x, cost in enumerate(line.strip()):
                grid[x + y * 1j] = int(cost)
        pprint(grid)

        # Create original grid
        max_y = int(max(coord.imag for coord in grid))
        max_x = int(max(coord.real for coord in grid))
        big_grid = {}
        for coord, value in grid.items():
            for y in range(5):
                for x in range(5):
                    big_grid[coord + x * (max_x + 1) + y * (max_y + 1) * 1j] = value

        # Create bigger grid
        big_max_y = int(max(coord.imag for coord in big_grid))
        big_max_x = int(max(coord.real for coord in big_grid))
        for y in range(big_max_y + 1):
            for x in range(big_max_x + 1):
                coord = x + y * 1j
                left = coord - (max_x + 1)
                top = coord - (max_y + 1) * 1j

                value = big_grid[coord]
                if left in big_grid:
                    value = (big_grid[left] + 1) % 10 or 1
                elif top in big_grid:
                    value = (big_grid[top] + 1) % 10 or 1
                big_grid[coord] = value

        print(len(big_grid))
        pprint(big_grid)
        dijkstra(big_grid)


if __name__ == "__main__":
    solve1()

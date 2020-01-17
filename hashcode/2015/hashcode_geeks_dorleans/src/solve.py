#! /usr/bin/env python
# -*- coding: utf-8 -*-


from __future__ import print_function
from collections import defaultdict
import numpy as np
from row import Row
from score import score_solution, score_result


def build_rows(nb_rows, unavailables_slots, nb_slots_per_row):
    unavailables = defaultdict(list)
    for (line, slot) in unavailables_slots:
        unavailables[line].append(slot)
    rows = [Row(i, nb_slots_per_row, unavailables[i]) for i in range(nb_rows)]
    rows.sort(reverse=True, key=lambda r: r.getmax())
    return rows


def solve(data):
    # Servers [(id, capacity, size)]
    # Sort servers by capacity / size
    servers = sorted(
        data.servers,
        reverse=True,
        key=lambda (_, capacity, size): (capacity / size, 1 / (1 + float(size)))
    )

    # Build a list of rows
    rows = build_rows(data.rows, data.unavailables, data.slots)

    # Result is a mapping from id to (server_size, server_capacity, row_id, server_pos, group_id)
    result = np.empty((len(data.servers), 5))
    for (server_id, capacity, size) in servers:
        result[server_id, :] = (size, capacity, -1, -1, -1)

    # For each row, compute the group capacity for each group
    group_capacity_per_row = np.zeros((data.rows, data.pools))
    # Partial score
    partial_score_per_row = np.zeros((data.rows, data.pools))

    # Assign servers to lines
    for (server_id, capacity, size) in servers:
        # Get ID of group with lowest score
        best_group_id, _ = score_solution(partial_score_per_row)

        # Rows with enough space to fit the server
        possible_rows = [
            (row, group_capacity_per_row[row.index, best_group_id]) for row in rows
            if size in row
        ]

        if len(possible_rows) == 0:
            continue

        # Sort by group score
        possible_rows.sort(key=lambda (row, score): (score, min(group_capacity_per_row[row.index, :])))
        # Take the 5 rows with the lowest group on it
        best_x_rows = possible_rows[:min(len(possible_rows), 5)]
        # Choose the line with the maximum number of free slots
        best_x_rows.sort(key=lambda (row, _): row.freeslots(), reverse=True)
        best_row, _ = best_x_rows[0]

        # Fit server in best row
        server_pos = best_row.useslot(size)

        # Update capacity of each group, on best row
        group_capacity_per_row[best_row.index, best_group_id] += capacity
        partial_score_per_row[:, best_group_id] += capacity
        partial_score_per_row[best_row.index, best_group_id] -= capacity

        # Update result
        result[server_id, 2:] = (best_row.index, server_pos, best_group_id)

    _, total_score = score_solution(partial_score_per_row)
    return total_score, result

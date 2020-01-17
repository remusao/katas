#! /usr/bin/env python
# -*- coding: utf-8 -*-


import numpy as np


def score_solution(groups_scores):
    min_group_score = groups_scores.min(axis=0)
    min_group_id = min_group_score.argmin()
    return min_group_id, min_group_score[min_group_id]


def score_result(result, nb_servers, nb_rows, nb_pools):
    result = result.reshape((nb_servers, 5))
    # For each row, compute the group capacity for each group
    group_capacity_per_row = np.zeros((nb_rows, nb_pools))
    # For each gorup, compute its total capacity
    capacities_of_groups = np.zeros((nb_pools,))

    # Result is a mapping from id to (server_size, server_capacity, row_id, server_pos, group_id)
    for (_, capacity, row_id, _, group_id) in result:
        if row_id != -1 and group_id != -1:
            group_capacity_per_row[row_id, group_id] += capacity
            capacities_of_groups[group_id] += capacity
    groups_scores = np.tile(capacities_of_groups, (nb_rows, 1)) - group_capacity_per_row
    return score_solution(groups_scores)[1]

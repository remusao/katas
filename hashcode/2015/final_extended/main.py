

from __future__ import print_function
import sys
import numpy as np
import random
from itertools import islice, product
from collections import namedtuple, defaultdict


#
# Model
#


Problem = namedtuple("Problem", [
    "R", "C", "A", "L", "V", "B", "T", "Rs", "Cs",
    "targets",
    "dests",
    "cov"
])


class Loon(object):
    def __init__(self, row, col):
        self.row = row
        self.col = col
        self.layer = -1
        self.history = []
        self.out = False


# def simulate(p, result):
#     loons = [Loon(p.Rs, p.Cs) for _ in range(p.B)]
#     total_score = 0
#     # For each simulation step
#     for moves in result:
#         # Move each balloon
#         covered = set()
#         for move, loon in zip(moves, loons):
#             loon.layer += move
#             if loon.layer >= 0:
#                 loon.row, loon.col = p.layers[loon.layer, loon.row, loon.col]
#                 covered.update(p.cov[loon.row, loon.col])
#         total_score += len(covered)
#     return total_score


#
# Solve
#


def columnsdist(c1, c2, C):
    return min(abs(c1 - c2), C - abs(c1 - c2))


def iscovered(target, row, col, C, V):
    u, v = target
    return (row - u) ** 2 + columnsdist(col, v, C) ** 2 <= V * V


def compute_cov(R, C, targets, loon_radius):
    """ For each cell in the grid, associated covered targets. """
    cov = defaultdict(list)
    for row in range(R):
        for col in range(C):
            for target_id, (target_row, target_col) in enumerate(targets):
                if iscovered((target_row, target_col), row, col, C, loon_radius):
                    cov[row, col].append(target_id)
    return cov


def simulate(p, b, solution, covered, decision):
    score = 0
    r, c, a = p.Rs, p.Cs, 0
    for t in range(p.T):
        bestda = decision(b, t, a, r, c)
        solution[t, b] = bestda
        a += bestda;
        r, c = p.dests[a, r, c]
        if r < 0:
            break
        elif a > 0:
            for target in p.cov[r, c]:
                score += 0 if covered[t + 1, target] else 1
                covered[t + 1, target] = True
    return score


def get_path(p, dirs, best, covered, b):
    left = np.zeros((p.T + 1, p.R, p.C), dtype=int)
    for t in range(p.T):
        for r in range(p.R):
            for c in range(p.C):
                score = 0
                for target in p.cov[r, c]:
                    score += 0 if covered[t + 1, target] else 1
                left[t, r, c] = score

    for t in range(p.T - 1, -1, -1):
        for a in range(p.A + 1):
            for r in range(p.R):
                for c in range(p.C):
                    bestda, bestv = 0, 0
                    for da in (-1, 0, 1):
                        if a <= 1 and da == -1:
                            continue
                        elif a + da > p.A:
                            continue
                        next = p.dests[a + da, r, c]
                        if next[0] < 0:
                            break
                        rscore = best[t + 1, a + da, next[0], next[1]]
                        if a + da > 0:
                            rscore += left[t + 1, next[0], next[1]]
                        if rscore > bestv or (rscore == bestv and random.randint(0, 1)):
                            bestv = rscore
                            bestda = da
                    best[t, a, r, c] = bestv
                    dirs[t, a, r, c] = bestda


def solve(p):
    solution = np.zeros((p.T, p.B), dtype=np.int8)
    best = np.zeros((p.T + 1, p.A + 1, p.R, p.C), dtype=int)
    dirs = np.zeros((p.T + 1, p.A + 1, p.R, p.C), dtype=np.int8)
    for br in range(p.B):
        print("Loon", br)
        covered = np.zeros((p.T + 1, p.L), dtype=np.bool_)
        score = 0
        for b in range(p.B):
            if b != br:
                score += simulate(p, br, solution, covered,
                                  lambda b, t, a, r, c: solution[t, b])
        get_path(p, dirs, best, covered, br)
        score += simulate(p, br, solution, covered, lambda b, t, a, r, c: dirs[t, a, r, c])
        yield score, solution


#
# IO
#


def read_input(path):
    """ Parse problem statement from file. """
    with open(path, "rb") as input_file:
        content = iter(map(int, input_file.read().split()))
        R, C, A, L, V, B, T, Rs, Cs = islice(content, 9)
        targets = [tuple(islice(content, 2)) for _ in range(L)]
        dests = np.zeros((A + 1, R, C, 2), dtype=np.int8)
        for (layer, row, col) in product(range(A + 1), range(R), range(C)):
            if layer == 0:
                dests[layer, row, col] = (row, col)
            else:
                dir_r, dir_c = tuple(islice(content, 2))
                dest_r, dest_c = dir_r + row, (dir_c + col) % C
                if not (0 <= dest_r < R):
                    dest_r, dest_c = -1, -1
                dests[layer, row, col] = (dest_r, dest_c)
        cov = compute_cov(R, C, targets, V)
        return Problem(R, C, A, L, V, B, T, Rs, Cs, targets, dests, cov)


def dump_solution(result, output_filename="output.sol", prefix=""):
    """ Given a list of steps, where each step consists in moves for each loon,
    dump the result in a file with the good format. """
    output_path = "%s_%s" % (prefix, output_filename)
    with open(output_path, 'wb') as output:
        for step in range(len(result)):
            moves = result[step]
            print(' '.join(map(str, moves)), file=output)


#
# Entry point
#


def main():
    print("Parse input")
    problem_statement = read_input(sys.argv[1])
    max_score = 0
    for iteration in range(10):
        print("[%s] Solve problem" % iteration)
        for (score, result) in solve(problem_statement):
            if score > max_score:
                max_score = score
                print("[%s] score: %s" % (iteration, score))
                print("[%s] Dump solution" % iteration)
                dump_solution(result, prefix=str(score))


if __name__ == "__main__":
    main()




from __future__ import print_function
import numpy as np
import random
import time
import math
from collections import defaultdict


class Earth(object):
    def __init__(self, R, C, A, L, V, B, T, Rs, Cs, targets, layers):
        # Parameters of the simulation
        self.n_rows = R
        self.n_cols = C
        self.n_layers = A
        self.n_targets = L
        self.balloon_radius = V
        self.n_loons = B
        self.n_steps = T
        self.loon_mask_rows, self.loon_mask_cols = self.loon_scope(self.balloon_radius)
        self.Rs = Rs
        self.Cs = Cs

        print(R, "rows")
        print(C, "cols")
        print(A, "layers")

        self.targets = np.zeros((R, C), dtype=bool)
        for target_no in range(len(targets)):
            (tr, tc) = targets[target_no]
            self.targets[tr, tc] = 1

        # List of balloons
        self.loons = []
        for b_no in range(self.n_loons):
            self.loons.append(Balloon((int(Rs), int(Cs), -1)))

        # Get the rewards
        self.rewards = self.get_rewards(targets, V)
        # Array of cells
        # (x, y) -> cell
        self.earth = {}
        for row in range(self.n_rows):
            for col in range(self.n_cols):
                reward = self.rewards[row, col]
                dirs = self.get_dirs(row, col, layers)
                transition_probas = [(0.3, 0.3, 0.3) for _ in range(8)]
                self.earth[(row, col)] = Cell(
                    reward,
                    dirs,
                    transition_probas
                )

    def loon_scope(self, radius):
        def columnsdist(c1, c2):
            return min(abs(c1 - c2), self.n_cols - abs(c1 - c2))
        rows = []
        cols = []
        for row in range(-radius, radius, 1):
            for col in range(-radius, radius, 1):
                if (row ** 2 + col ** 2) <= radius ** 2:
                    rows.append(row)
                    cols.append(col)
        return rows, cols

    def get_score(self):
        def columnsdist(c1, c2):
            return min(abs(c1 - c2), self.n_cols - abs(c1 - c2))
        mask = np.zeros((self.n_rows, self.n_cols), dtype=bool)
        for loon in self.loons:
            if loon.out:
                continue

            ti = loon.row
            tj = loon.col
            for i in range(-self.balloon_radius, self.balloon_radius + 1):
                for j in range(-self.balloon_radius, self.balloon_radius + 1):
                    if (i + ti < self.n_rows) and (i + ti >= 0):
                        if (i) ** 2 + columnsdist(tj, j + tj) ** 2 <= self.balloon_radius ** 2:
                            mask[ti + i, (tj + j) % self.n_cols] = 1
        return np.count_nonzero((self.targets & mask))

    def get_dirs(self, row, col, layers):
        dirs = []
        for layer in layers:
            dirs.append(layer[row, col, :])
        return dirs

    def get_rewards(self, targets, balloon_radius):
        rewards = np.zeros((self.n_rows, self.n_cols))
        def columnsdist(c1, c2):
            return min(abs(c1 - c2), self.n_cols - abs(c1 - c2))

        for target_no in range(len(targets)):
            (ti, tj) = targets[target_no]
            for i in range(-balloon_radius, balloon_radius + 1):
                for j in range(-balloon_radius, balloon_radius + 1):
                    if (i + ti < self.n_rows) and (i + ti >= 0):
                        if (i) ** 2 + columnsdist(tj, j + tj) ** 2 <= balloon_radius ** 2:
                            rewards[ti + i, (tj + j) % self.n_cols] += 1
        return rewards

class Cell(object):
    def __init__(self, reward, dirs, probas):
        # Number of targets covered
        # Int
        self.reward = reward
        # for each layer, contains the next cell
        # [(x, y)]
        self.dirs = dirs
        # For each layer, contains a triplet [(p-1, p0, p+1)]
        self.transitions_probas = probas

        # Initialize the extrem layers for the forbiding moves.
        self.transitions_probas[0] = (0, 0.5, 0.5)
        self.transitions_probas[len(probas) - 1] = (0.5, 0.5, 0)

class Balloon(object):
    def __init__(self, position):
        # History of [(decision, x, y, z)]
        # decision = -1, 0 or 1
        # x, y, z = new position after decision and wind effect
        self.history = []
        self.row, self.col, self.layer = position
        self.out = False

    def update(self, decision, cell, earth):
        self.row = int(cell.dirs[self.layer + decision][0] + self.row)
        self.col  = int((cell.dirs[self.layer + decision][1] + self.col) % earth.n_cols)

        self.layer += decision
        # if balloon is out, we set to out.
        if (self.row < 0) or (self.row >= earth.n_rows):
            self.out = True
            reward = 0
        else:
            reward = earth.rewards[self.row, self.col]
        self.history.append((decision, self.row, self.col, self.layer, reward, self.out))

def can_take_off(loon):
    return random.randint(0, 1)

def take_decision(cell, loon):
    r = random.random()

    if loon.out:
        return 0

    # If loon is on earth, we must take the decision of the take off.
    if loon.layer == -1:
        return can_take_off(loon)
    # If loon is in air, we take normal decision.
    if (r < cell.transitions_probas[loon.layer][0]) and loon.layer > 0:
        return -1
    elif (r > cell.transitions_probas[loon.layer][0] + cell.transitions_probas[loon.layer][1]) and loon.layer < (7):
        return 1
    else:
        return 0

def solve(args):
    result = []
    print("Init earth")
    earth = Earth(*args)

    # Init global scores
    print("Init global scores")
    scores = {}
    for row in range(earth.n_rows):
        for col in range(earth.n_cols):
            for layer in range(earth.n_layers):
                for action in (-1, 0, 1):
                    next_row, next_col = earth.earth[row, col].dirs[layer]
                    reward = earth.rewards[next_row, next_col]
                    # scores[(row, col, layer, action)] = (reward, 0)
                    if (0 == layer and action == -1) or (layer == (earth.n_layers - 1) and action == 1):
                        scores[(row, col, layer, action)] = (0, 0)
                    else:
                        scores[(row, col, layer, action)] = (float(earth.n_targets) / float(earth.n_cols * earth.n_rows), 0)

    print("Begin simu")
    t0 = time.time()
    for metastep in range(10000):
        print("Metastep", metastep)
        loon = Balloon((int(earth.Rs), int(earth.Cs), -1))
        for step in range(earth.n_steps):
            # Take decision (-1, 0 or +1)
            if not loon.out:
                cell = earth.earth[(int(loon.row), int(loon.col))]
                decision = take_decision(cell, loon)
                loon.update(decision, cell, earth)
            else:
                break
        history = loon.history
        # Compute total score for the loon
        loon_score = 0
        for (_, _, _, _, reward, out) in history:
            if not out:
                loon_score += reward
        S = float(loon_score) / float(earth.n_steps)
        # Update sum and count on intermediate nodes
        for (action, row, col, layer, reward, out) in history:
            if not out and layer >= 0:
                score_sum, count = scores[(int(row), int(col), layer, action)]
                if count > 0:
                    scores[(row, col, layer, action)] = (score_sum + S, count + 1)
                else:
                    scores[(row, col, layer, action)] = (S, 1)
    print(time.time() - t0, "seconds")

    # Update probas
    for row in range(earth.n_rows):
        for col in range(earth.n_cols):
            for layer in range(earth.n_layers):
                probas = {}
                for action in (-1, 0, 1):
                    V, Vu = scores[(row, col, layer, action)]
                    if Vu > 0:
                        probas[action] = math.exp(float(V) / float(Vu))
                    elif V == 0:
                        probas[action] = 0
                    else:
                        probas[action] = math.exp(float(V))

                proba_sum = float(sum(probas.values()))
                new_probas = [
                    probas[-1] / proba_sum,
                    probas[0] / proba_sum,
                    probas[1] / proba_sum
                ]
                if layer == 0:
                    new_probas[0] = 0
                elif layer == (earth.n_layers - 1):
                    new_probas[2] = 0
                earth.earth[row, col].transitions_probas[layer] = new_probas

    while True:
        score = 0
        for step in range(earth.n_steps):
            print("Step", step)
            moves = []
            for loon_id, loon in enumerate(earth.loons):
                # Take decision (-1, 0 or +1)
                if loon.out:
                    decision = 0
                else:
                    cell = earth.earth[(int(loon.row), int(loon.col))]
                    decision = take_decision(cell, loon)
                moves.append(decision)
                loon.update(decision, cell, earth)
            score += earth.get_score()

            result.append(moves)
            yield score, result

#!/usr/bin/env python3

from enum import IntEnum


class Moves(IntEnum):
    Rock = 1
    Paper = 2
    Scissors = 3


def main():
    moves = {
        "A": Moves.Rock,
        "B": Moves.Paper,
        "C": Moves.Scissors,
        "X": Moves.Rock,
        "Y": Moves.Paper,
        "Z": Moves.Scissors,
    }

    winning = {
        Moves.Rock: Moves.Scissors,
        Moves.Paper: Moves.Rock,
        Moves.Scissors: Moves.Paper,
    }

    loosing = {
        Moves.Rock: Moves.Paper,
        Moves.Paper: Moves.Scissors,
        Moves.Scissors: Moves.Rock,
    }

    # Part 1
    with open("./input.txt") as inputs:
        score = 0
        for line in inputs:
            # Decode moves
            omove, mmove = map(lambda l: moves[l], line.strip().split())

            # Add our shape score + outcome of the round score
            score += mmove + (
                3 if omove == mmove else 6 if omove == winning[mmove] else 0
            )

        print("Part 1:", score)

    # Part 2
    with open("./input.txt") as inputs:
        score = 0
        for line in inputs:
            omove, outcome = line.strip().split()
            omove = moves[omove]

            if outcome == "X":
                mmove = winning[omove]
            elif outcome == "Y":
                mmove = omove
                score += 3
            elif outcome == "Z":
                mmove = loosing[omove]
                score += 6

            # Add our shape score
            score += mmove

        print("Part 2:", score)


if __name__ == "__main__":
    main()

#!/usr/bin/env python


def transform(subject_number, loop_size):
    result = 1
    for _ in range(loop_size):
        result *= subject_number
        result %= 20201227
    return result


def guess_loop_size(subject_number, pubkey):
    result = 1
    loop_size = 0
    while result != pubkey:
        result *= subject_number
        result %= 20201227
        loop_size += 1
    return loop_size


def solve():
    # Example
    card_pubkey = 5764801
    door_pubkey = 17807724

    # Input
    card_pubkey = 14205034
    door_pubkey = 18047856

    card_loop_size = guess_loop_size(subject_number=7, pubkey=card_pubkey)
    print(card_loop_size)
    door_loop_size = guess_loop_size(subject_number=7, pubkey=door_pubkey)
    print(door_loop_size)

    print(transform(subject_number=card_pubkey, loop_size=door_loop_size))
    print(transform(subject_number=door_pubkey, loop_size=card_loop_size))


if __name__ == "__main__":
    solve()

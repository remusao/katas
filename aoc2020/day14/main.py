#!/usr/bin/env python

import time

def solve1(lines):
    mask = "X" * 36
    mask_or = int("0" * 36, 2)
    mask_and = int("1" * 36, 2)

    mem = {}
    for instr in lines:
        if instr.startswith("mask = "):
            mask = instr.split(" = ", 1)[-1].strip()
            mask_or = int(mask.replace("X", "0"), 2)
            mask_and = int(mask.replace("X", "1"), 2)
        else:
            addr, value = map(int, instr[4:].split("] = ", 1))
            mem[addr] = (value | mask_or) & mask_and

    return sum(mem.values())


def combinations(mask, idx=0):
    if idx == len(mask):
        yield ""
    elif mask[idx] == "X":
        for n in combinations(mask, idx + 1):
            yield f"0{n}"
            yield f"1{n}"
    else:
        for n in combinations(mask, idx + 1):
            yield f"{mask[idx]}{n}"


def solve2(lines):
    mem = {}
    mask = None
    for instr in lines:
        if instr.startswith("mask = "):
            mask = instr.split(" = ", 1)[-1].strip()
        else:
            addr, value = map(int, instr[4:].split("] = ", 1))
            b = bin(addr)[2:]
            addrb = "0" * (36 - len(b)) + b
            addr2 = "".join([b if m == "0" else m for (b, m) in zip(addrb, mask)])
            for naddr in combinations(addr2):
                mem[int(naddr, 2)] = value

    return sum(mem.values())


if __name__ == "__main__":
    with open("./input1.txt") as inputs:
        lines = list(inputs)

        # Warm-up
        for _ in range(20):
            solve1(lines)
            solve2(lines)

        t0 = time.time()
        solve1(lines)
        solve2(lines)
        t1 = time.time()
        print("Time:", t1 - t0)

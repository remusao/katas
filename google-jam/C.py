

from __future__ import unicode_literals, print_function
import numpy as np
import sys

I = 2
J = 3
K = 4

quat2int = {
    "i": I,
    "j": J,
    "k": K
}

DMULT = {
    (1, 1): 1,
    (1, I): I,
    (1, J): J,
    (1, K): K,

    (I, 1): I,
    (I, I): -1,
    (I, J): K,
    (I, K): -J,

    (J, 1): J,
    (J, I): -K,
    (J, J): -1,
    (J, K): I,

    (K, 1): K,
    (K, I): J,
    (K, J): -I,
    (K, K): -1,
}

MULT = np.zeros((4, 4), dtype=np.int8)
for ((a, b), res) in DMULT.items():
    MULT[a - 1, b - 1] = res


def mult(a, b):
    aabs = abs(a)
    babs = abs(b)
    return (MULT[aabs - 1, babs - 1] * (a / aabs) * (b / babs))


def solve(pattern, offset=0, goali=0):
    goals = [I, J, K]
    length = len(pattern)
    while (goali < 3) and (offset < (length - 1)):
        if pattern[offset] == goals[goali]:
            offset += 1
            if goali < 2:
                if solve(pattern, offset, goali + 1):
                    return True
            else:
                return False
        else:
            pattern[offset + 1] = mult(pattern[offset], pattern[offset + 1])
            offset += 1
    return (offset == length) and (goali == 2) and (pattern[-1] == goals[goali])


def main():
    with open(sys.argv[1], "rb") as input_file:
        T = int(next(input_file).strip())
        for t in range(1, T + 1):
            _, X = map(int, next(input_file).split())
            pattern = [quat2int[q] for q in next(input_file).strip()] * X
            result = "YES" if solve(pattern) else "NO"
            print("Case #{t}: {result}".format(
                t=t,
                result=result))



if __name__ == "__main__":
    main()

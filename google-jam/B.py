

from __future__ import unicode_literals, print_function
import numpy as np
import sys


def main():
    with open(sys.argv[1], "rb") as input_file:
        T = int(next(input_file).strip())
        for t in range(1, T + 1):
            _ = next(input_file)

            result = 0
            total = 0
            # D[i] = number of pancakes of diner #i
            diners = np.array(map(int, next(input_file).split()))
            # P[i] = number of diners with i pancakes
            pancakes = np.zeros((max(diners) + 1,), dtype=int)
            for p in diners:
                pancakes[p] += 1
                total += p
            # C[i] = number of diners with at least i pancakes
            cumul = np.zeros(pancakes.shape, dtype=int)
            cumul[-1] = pancakes[-1]
            for i in range(len(pancakes) - 2, 0, -1):
                cumul[i] = cumul[i + 1] + pancakes[i]

            # print("P", pancakes)
            while total > 0:
                # print("D", diners)
                # print("C", cumul)
                # Decide if we must do a special step
                # Split gain (what do we gain by splitting pancakes)
                sgain = np.zeros((len(diners),), dtype=int)
                sorted_index = np.argsort(diners)[::-1]
                for i in range(len(sorted_index) - 1):
                    sgain[i] = diners[sorted_index[i]] - diners[sorted_index[i + 1]]
                sgain[-1] = diners[sorted_index[-1]]
                # print("G", sgain)
                # Compare cumulative sums of sgain and cumul
                remaining_steps = len(cumul) - 1
                while cumul[remaining_steps] == 0:
                    remaining_steps -= 1
                remaining_steps -= 1
                sgc, cc = 0, 0
                for i in range(min(len(sgain), remaining_steps)):
                    if sgc > cc:
                        break
                    sgc += sgain[i]
                    cc += cumul[i + 1]
                special = sgc > cc # np.any(sgain[:] > cumul[1:1 + min(len(sgain), remaining_steps)])
                if special:
                    # print("special")
                    num_p = diners[sorted_index[0]]
                    halfp = num_p / 2
                    diners[sorted_index[0]] -= halfp
                    diners = np.append(diners, halfp)
                    for i in range(num_p, diners[sorted_index[0]], -1):
                        cumul[i] -= 1
                    for i in range(halfp, 0, -1):
                        cumul[i] += 1
                else:
                    # print("normal")
                    diners[diners > 0] -= 1
                    total -= cumul[1]
                    cumul[:-1] = cumul[1:]
                    cumul[len(cumul) - result:] = 0
                result += 1
            print("Case #{t}: {result}".format(t=t, result=result))



if __name__ == "__main__":
    main()

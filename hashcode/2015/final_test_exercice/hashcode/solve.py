


from __future__ import print_function
import numpy as np
import ipdb

HAM = 1
REST = 0


def pizz_slice(pizza, y, x, sx, sy):
    return pizza[y:y+sy, x:x+sx]


def solve1(pizza, ham_size, size_max):
    size_max = 3
    result = []
    height, width = pizza.shape
    print(width, height)
    # Window of size S and count number of Ham in every time
    for i in range(0, height - size_max, size_max):
        for j in range(0, width - size_max, size_max):
            slc = pizz_slice(pizza, i, j, size_max)
            print(slc)
            count = np.count_nonzero(slc)
            if count >= ham_size:
                result.append((i, j, i + size_max - 1, j + size_max - 1))
                print(result[-1])
    return result


def solve2(pizza, ham_size, max_size):
    def subsolve(sx, sy):
        mask = np.zeros(pizza.shape)
        result = []
        height, width = pizza.shape
        # Window of size S and count number of Ham in every time
        i = 0
        while i <= height - sy:
            j = 0
            while j <= width - sx:
                slc = pizz_slice(pizza, i, j, sx, sy)
                count = np.count_nonzero(slc)
                if count >= ham_size:
                    result.append((i, j, i + sy - 1, j + sx - 1))
                    mask[i:i+sy, j:j+sx] = 1
                    j += sx
                else:
                    j += 1
            i += sy
        print(np.count_nonzero(mask))
        return result

    all_solutions = []
    for sx in range(1, max_size):
        for sy in range(1, max_size):
            if sy * sx <= max_size:
                all_solutions.append(subsolve(sx, sy))
    return max(all_solutions, lambda p: len(p))


def solve(pizza, ham_count, max_size):
    def subsolve(sy_init, sx_init):
        sx = sx_init
        sy = sy_init
        mask = np.zeros(pizza.shape)
        result = []
        height, width = pizza.shape
        # Window of size S and count number of Ham in every time
        y = 0
        while y <= height - sy:
            x = 0
            while (x + sx) < width:
                slc = pizz_slice(pizza, y, x, sx, sy)
                count = np.count_nonzero(slc)
                if count >= ham_count:
                    result.append(((y, x, y + sy - 1, x + sx - 1), sx * sy))
                    mask[y:y+sy, x:x+sx] = 1
                    x += sx
                    sx = sx_init
                elif (sx + 1) * sy <= max_size:
                    sx += 1
                else:
                    x += 1
                    sx = sx_init
            y += sy
            x = 0
        result2 = []
        for (y, x, y2, x2), area in result:
            while (area + sy) <= max_size:
                if (mask[y:y2+1,x2+1] == 0).all():
                    x -= 1
                    area += sy
                    mask[y:y2+1,x2+1] = 1
                else:
                    break
            result2.append((y, x, y2, x2))


        return result, np.count_nonzero(mask)

    sols = []
    for sy in range(1, max_size):
        for sx in range(1, max_size):
            if sx * sy <= max_size:
                sols.append(subsolve(sy, sx))
    result, score = max(sols, key=lambda (p, s): s)
    print(score)
    return result

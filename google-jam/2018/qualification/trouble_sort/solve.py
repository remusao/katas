
import sys


def main(fd):
    lines = iter(fd.read().split('\n'))
    t = int(next(lines))
    for j in range(1, t + 1):
        n = int(next(lines))

        # Read list of numbers
        arr = list(map(int, next(lines).strip().split(' ')))

        eve = arr[::2]
        odd = arr[1::2]

        eve.sort()
        odd.sort()

        arr = eve + odd
        arr[::2] = eve
        arr[1::2] = odd

        not_sorted = -1
        for i in range(1, n):
            if (arr[i - 1] > arr[i]):
                not_sorted = i - 1
                break

        print('Case #{n}: {answer}'.format(
            n=j,
            answer='OK' if not_sorted == -1 else not_sorted
        ))


if __name__ == "__main__":
    main(sys.stdin)
    # with open('./load_test_big.in', 'rt') as fd:
    #     main(fd)

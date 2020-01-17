

from __future__ import unicode_literals, print_function
import sys


def main():
    with open(sys.argv[1], "rb") as input_file:
        T = int(next(input_file).strip())
        for t in range(1, T + 1):
            _, people = next(input_file).split()
            people = [int(s) for s in people]
            result = 0
            count = 0
            for i, s in enumerate(people):
                if count < i:
                    result += i - count
                    count += i - count
                count += s
            print("Case #{t}: {result}".format(t=t, result=result))



if __name__ == "__main__":
    main()


import random


def main():
    n = 100
    v = 100000
    maxi = 1000000000
    print(n)
    for _ in range(n):
        print(v)
        print(' '.join(str(int(maxi * random.random())) for i in range(v)))

if __name__ == "__main__":
    main()

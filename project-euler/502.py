
import time
import random
import sys

def display(heights):
    cols = range(31, 37)
    h = max(*heights)
    w = len(heights)
    print(h, w)
    for _ in range(h):
        for i in range(w):
            if heights[i] > 0:
                heights[i] -= 1
                colb = '\033[%sm' % cols[heights[i] % len(cols)]
                sys.stdout.write('%s#\033[37m' % colb)
            else:
                sys.stdout.write(' ')
        print ''


def main():
    w, h = 4, 4
    print("%s x %s" % (w, h ))

    for level in range(h):
        for w1 in range(1, w + 1):
            n = 1
            while (n * w1) <= (w - n + 1):
                print("%s: %s x %s" % (level, n, w1))
                n += 1


if __name__ == "__main__":
    main()
    while True:
        display([random.randint(0, 37) for r in range(150)])
        print ''

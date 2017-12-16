import sys


def main(f):
    _t = int(f.readline())
    for t in xrange(0, _t):
        s = 0
        N = int(f.readline())
        men = map(int, f.readline().split())
        women = map(int, f.readline().split())
        men.sort()
        women.sort()
        for i in xrange(0, N):
            s += men[i] * women[i]
        print s


if __name__ == '__main__':
    f = sys.stdin
    main(f)

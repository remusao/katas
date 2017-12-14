
import sys
from itertools import count, islice

def severities(firewall, delay=0):
    return ((v / 2 + 1) * i for (i, v) in firewall if (i + delay) % v == 0)

def solve1(firewall):
    return sum(severities(firewall))

def solve2(firewall):
    return next(d for d in count() if next(severities(firewall, d), None) is None)

def main():
    firewall = []
    for line in sys.stdin.read().strip().splitlines():
        d, r = line.split(': ')
        firewall.append((int(d), 2 * (int(r) - 1)))

    print(solve1(firewall))
    print(solve2(firewall))

if __name__ == '__main__':
    main()

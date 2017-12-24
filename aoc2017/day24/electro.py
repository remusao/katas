
from collections import defaultdict, Counter
import sys

def read_ports():
    ports = defaultdict(Counter)
    for line in sys.stdin.read().strip().splitlines():
        start, end = map(int, line.split('/'))
        ports[start][end] += 1
        ports[end][start] += 1
    return ports

def bridges(ports, p=0):
    for port, c in list(ports[p].items()):
        if c > 0:
            ports[p][port] -= 1
            ports[port][p] -= 1
            for bridge in bridges(ports, port):
                yield [p, port] + bridge
            ports[p][port] += 1
            ports[port][p] += 1
    yield []

def solve1(ports, p=0):
    return max(sum(bridge) for bridge in bridges(ports))

def solve2(ports, p=0):
    return max((len(bridge), sum(bridge)) for bridge in bridges(ports))[1]

def main():
    ports = read_ports()
    print(solve1(ports))
    print(solve2(ports))

if __name__ == '__main__':
    main()

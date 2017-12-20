

from itertools import count
from collections import defaultdict
import sys

def read_particles():
    return [
        tuple([
            tuple(map(int, part.strip()[3:-1].split(',')))
            for part in line.split(', ')
        ]) for line in sys.stdin.read().splitlines()
    ]

def manhattan(p):
    return abs(p[0]) + abs(p[1]) + abs(p[2])

def solve1(particles):
    return min(
        range(len(particles)),
        key=lambda p: (manhattan(particles[p][2]), manhattan(particles[p][0]))
    )

def position(particle, t):
    ((px, py, pz),
     (vx, vy, vz),
     (ax, ay, az),
    ) = particle

    return (
        px + t * vx + ax * t * (t + 1) // 2,
        py + t * vy + ay * t * (t + 1) // 2,
        pz + t * vz + az * t * (t + 1) // 2,
    )

def solve2(particles):
    farthest = max(manhattan(p[0]) for p in particles)

    for t in count():
        collisions = defaultdict(list)
        for p in particles:
            collisions[position(p, t)].append(p)

        particles = [g[0] for g in collisions.values() if len(g) == 1]
        if min(manhattan(p) for p in collisions) > farthest:
            break

    return len(particles)

def main():
    particles = read_particles()
    print(solve1(particles))
    print(solve2(particles))


if __name__ == '__main__':
    main()

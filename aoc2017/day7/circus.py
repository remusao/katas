

from collections import Counter, defaultdict
import sys

def load_programs():
    programs = {}
    for line in sys.stdin.read().splitlines():
        name, weight, *rest = line.split()
        children = []
        if rest:
            children = [name.strip(',') for name in rest[1:]]
        programs[name] = (
            name,
            int(weight[1:-1]),
            children
        )
    return programs


def build_tree(programs, node=None):
    # Detect root
    if node is None:
        node = solve1(programs)

    tree = list(programs[node])
    tree[-1] = [
        build_tree(programs, child)
        for child in tree[-1]
    ]
    return tuple(tree)

def recSolve2(tree):
    name, weight, children = tree
    weights = {
        c[0]: recSolve2(c)
        for c in children
    }
    total = sum(weights.values()) + weight

    if len(set(weights.values())) == 2:
        w2c = defaultdict(list)
        for child, w in weights.items():
            w2c[w].append(child)

        sorted_weights = sorted(w2c.items(), reverse=True, key=lambda e: len(e[1]))
        target_weight = sorted_weights[0][0]
        actual_weight = sorted_weights[1][0]
        diff = target_weight - actual_weight
        node_name = sorted_weights[1][1][0]
        print('looking for', node_name)
        for (n, w, _) in children:
            if n == node_name:
                print(n, w + diff)
                total += diff

    return total


def solve2(programs):
    tree = build_tree(programs)
    recSolve2(tree)

def solve1(programs):
    names = set(programs)
    for name, (_, _, children) in programs.items():
        for child in children:
            if child in names:
                names.remove(child)
    return next(iter(names))

def main():
    programs = load_programs()
    print(solve2(programs))

if __name__ == '__main__':
    main()

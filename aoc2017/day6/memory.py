
import sys


def main():
    blocks = tuple(map(int, sys.stdin.read().split()))
    count = 0
    seen = {}
    while blocks not in seen:
        print blocks
        seen[blocks] = count
        # Redistribute
        max_index = max(range(len(blocks)), key=lambda i: blocks[i])
        blocks = list(blocks)
        blocks[max_index], value = 0, blocks[max_index]
        max_index += 1
        for _ in range(value):
            blocks[max_index % len(blocks)] += 1
            max_index += 1
        blocks = tuple(blocks)
        count += 1
    print count - seen[blocks]


if __name__ == '__main__':
    main()

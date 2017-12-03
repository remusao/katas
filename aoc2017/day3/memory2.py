from itertools import islice


def generate_coordinates():
    x = 0
    y = 0
    n = 0
    memo = {}

    current = 1
    memo[(x, y)] = current

    yield (x, y), current

    while True:
        # New square
        n += 1
        x += 1
        current = (
            memo.get((x - 1, y), 0) +   # Left
            memo.get((x - 1, y + 1), 0) # Left-Top
        )
        memo[(x, y)] = current
        yield (x, y), current

        # Go up
        while y < n:
            y += 1
            current = (
                memo.get((x - 1, y), 0) +     # Left
                memo.get((x - 1, y + 1), 0) + # Left-Top
                memo.get((x - 1, y - 1), 0) + # Left-Down
                memo.get((x, y - 1), 0)       # Bottom
            )
            memo[(x, y)] = current
            yield (x, y), current

        # Go left
        while x > -n:
            x -= 1
            current = (
                memo.get((x + 1, y), 0) + # Back
                memo.get((x + 1, y - 1), 0) + # Back-down
                memo.get((x, y - 1), 0) + # Down
                memo.get((x - 1, y - 1), 0)  # Down-left
            )
            memo[(x, y)] = current
            yield (x, y), current

        # Go down
        while y > -n:
            y -= 1
            current = (
                memo.get((x, y + 1), 0) + # Top
                memo.get((x + 1, y + 1), 0) + # Top-right
                memo.get((x + 1, y), 0) + # Right
                memo.get((x + 1, y - 1), 0)  # Right-down
            )
            memo[(x, y)] = current
            yield (x, y), current

        # Go right
        while x < n:
            x += 1
            current = (
                memo.get((x - 1, y), 0) + # Back
                memo.get((x - 1, y + 1), 0) + # Back-top
                memo.get((x, y + 1), 0) + # Top
                memo.get((x + 1, y + 1), 0)  # Top-right
            )
            memo[(x, y)] = current
            yield (x, y), current


for _, s in generate_coordinates():
    if s > 289326:
        print(s)
        break

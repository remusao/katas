from itertools import islice


def generate_coordinates():
    x = 0
    y = 0
    n = 0

    yield (x, y)

    while True:
        # New square
        n += 1
        x += 1
        yield (x, y)

        # Go up
        while y < n:
            y += 1
            yield (x, y)

        # Go left
        while x > -n:
            x -= 1
            yield (x, y)

        # Go down
        while y > -n:
            y -= 1
            yield (x, y)

        # Go right
        while x < n:
            x += 1
            yield (x, y)

n, (x, y) = list(islice(enumerate(generate_coordinates()), 289326))[-1]
print(n + 1, (x, y), abs(x) + abs(y))

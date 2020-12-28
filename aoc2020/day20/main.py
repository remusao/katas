#!/usr/bin/env python

from collections import Counter, namedtuple
import itertools
import functools
import math

Tile = namedtuple("Tile", ["tid", "top", "bottom", "left", "right", "lines"])


def parse_tile(raw):
    lines = raw.split("\n")
    tid = int(lines[0][5:-1])
    top = lines[1]
    bottom = lines[-1]
    left = "".join(l[0] for l in lines[1:])
    right = "".join(l[-1] for l in lines[1:])
    return Tile(
        tid=tid, top=top, bottom=bottom, left=left, right=right, lines=tuple(lines[1:])
    )


def load_tiles():
    with open("./input1.txt") as inputs:
        return [parse_tile(raw) for raw in inputs.read().strip().split("\n\n")]


def rotate(tile):
    rotated_lines = [
        "".join(row[i] for row in tile.lines)
        for i in range(len(tile.lines) - 1, -1, -1)
    ]

    return Tile(
        tid=tile.tid,
        top=tile.right,
        bottom=tile.left,
        left=tile.top[::-1],
        right=tile.bottom[::-1],
        lines=tuple(rotated_lines),
    )  # Rotate 90


def test_rotate():
    # Size = 1
    assert rotate(
        Tile(tid=0, top="#", bottom="#", left="#", right="#", lines=("#",))
    ) == Tile(tid=0, top="#", bottom="#", left="#", right="#", lines=("#",))

    # Size = 2
    assert rotate(
        Tile(
            tid=0,
            top="-#",
            bottom=".-",
            left="-.",
            right="#-",
            lines=(
                "-#",
                ".-",
            ),
        )
    ) == Tile(
        tid=0,
        top="#-",
        bottom="-.",
        left="#-",
        right="-.",
        lines=(
            "#-",
            "-.",
        ),
    )


def tiles_rot_flip(tile):
    for flipped in [
        tile,  # Original
        Tile(
            tid=tile.tid,
            top=tile.bottom,
            bottom=tile.top,
            left=tile.left[::-1],
            right=tile.right[::-1],
            lines=tuple(tile.lines[::-1]),
        ),  # Horizontal
        Tile(
            tid=tile.tid,
            top=tile.top[::-1],
            bottom=tile.bottom[::-1],
            left=tile.right,
            right=tile.left,
            lines=tuple(row[::-1] for row in tile.lines),
        ),  # Vertical
    ]:
        yield flipped
        r1 = rotate(flipped)
        yield r1
        r2 = rotate(r1)
        yield r2
        yield rotate(r2)


def corners(tiles):
    counter = Counter(itertools.chain.from_iterable(tiles))
    return [
        tile
        for tile in tiles
        if (counter[tile.top] == 4 and counter[tile.left] == 4)
        or (counter[tile.top] == 4 and counter[tile.right] == 4)
        or (counter[tile.bottom] == 4 and counter[tile.left] == 4)
        or (counter[tile.bottom] == 4 and counter[tile.right] == 4)
    ]


def solve1(tiles):
    return math.prod({tile.tid for tile in corners(tiles)})


def finish_line(tiles, line, seen, remaining, previous=None):
    if remaining == 0:
        yield seen, line

    for next_tile in (
        tile
        for tile in tiles
        if line[-1].right == tile.left
        and tile.tid not in seen
        and (previous is None or previous[len(previous) - remaining].bottom == tile.top)
    ):
        yield from finish_line(
            tiles=tiles,
            line=line + [next_tile],
            seen=seen | {next_tile.tid},
            remaining=remaining - 1,
            previous=previous,
        )


def images(tiles, lines, remaining, seen):
    if remaining == 0:
        yield lines

    for next_tile in (
        tile
        for tile in tiles
        if tile.tid not in seen and tile.top == lines[-1][0].bottom
    ):
        for next_seen, next_line in finish_line(
            tiles=tiles,
            line=[next_tile],
            seen=seen | {next_tile.tid},
            remaining=11,
            previous=lines[-1],
        ):
            yield from images(
                tiles=tiles,
                lines=lines + [next_line],
                remaining=remaining - 1,
                seen=next_seen,
            )


def crop(image):
    res = []
    for line in image:
        for i in range(8):
            row = ""
            for tile in line:
                row += tile.lines[i + 1][1:-1]
            res.append(row)
    return tuple(res)


def solve2(tiles):
    monster = [
        (dx, dy)
        for dy, line in enumerate(
            [
                "                  # ",
                "#    ##    ##    ###",
                " #  #  #  #  #  #   ",
            ]
        )
        for dx, c in enumerate(line)
        if c == "#"
    ]

    n = 0
    total = 0
    for c in corners(tiles):
        for seen, line in finish_line(
            tiles=tiles, line=[c], seen={c.tid}, remaining=11
        ):
            for img in images(tiles=tiles, lines=[line], remaining=11, seen=seen):
                cropped = crop(img)
                total = sum([row.count("#") for row in cropped])
                for y, line in enumerate(cropped):
                    for x, c in enumerate(line):
                        if all(
                            cropped[y + dy][x + dx] == "#"
                            if (y + dy < len(cropped) and x + dx < len(cropped[0]))
                            else False
                            for (dx, dy) in monster
                        ):
                            n += 1

    # NOTE - on real input the total of '#' is 2444
    return total - n * len(monster)


def main():
    tiles = [t_ for t in load_tiles() for t_ in set(tiles_rot_flip(t))]
    print("Part 1:", solve1(tiles))
    print("Part 2:", solve2(tiles))


if __name__ == "__main__":
    main()

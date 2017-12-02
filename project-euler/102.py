
import sys

class Vertex(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y


def sign(p1, p2, p3):
    return (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y)


def PointInTriangle(pt, v1, v2, v3):
    b1 = sign(pt, v1, v2) < 0.0
    b2 = sign(pt, v2, v3) < 0.0
    b3 = sign(pt, v3, v1) < 0.0

    return ((b1 == b2) and (b2 == b3))


def main():
    count = 0
    orig = Vertex(0.0, 0.0)
    with open(sys.argv[1], "rb") as input_file:
        for line in input_file:
            v1x, v1y, v2x, v2y, v3x, v3y = map(int, line.strip().split(","))
            v1 = Vertex(v1x, v1y)
            v2 = Vertex(v2x, v2y)
            v3 = Vertex(v3x, v3y)
            if PointInTriangle(orig, v1, v2, v3):
                count += 1
    print count


main()

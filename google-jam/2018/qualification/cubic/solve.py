
import math


def rotate2d(x, y, angle):
    # Pre-compute
    theta = math.radians(angle)
    sin = math.sin(theta)
    cos = math.cos(theta)

    # print('theta', theta)
    # print('sin', sin)
    # print('cos', cos)

    return (
        x * cos - y * sin, # x
        x * sin + y * cos, # y
    )


def rotate(angle, vector, axis=0):
    """Counter-clockwise rotation around `axis`."""
    x, y, z = vector

    if axis == 0:
        # Rotate around x
        z, y = rotate2d(z, y, angle)
    elif axis == 1:
        # Rotate around y
        x, z = rotate2d(x, z, angle)
    elif axis == 2:
        # Rotate around z
        x, y = rotate2d(x, y, angle)

    return (x, y, z)

def rotate_points(angle, points, axis=0):
    return [
        rotate(angle, point, axis) for point in points
    ]


def scale(a, b):
    jump = (b - a) / 100.0
    while a < b:
        yield a
        a += jump


def shadow_area(corners, rotations):
    # By symmetry we can only consider the three following cases:
    # 1. we only see the top
    # 2. we see top + back (one rotation)
    # 3. we see top + back + right (two rotations)
    if rotations == 0:
        # We did not rotate the cube yet
        return 1.0
    elif rotations == 1:
        # We made only one rotation along z axis
        x_axis = [x for (x, _, _) in corners]
        min_x, max_x = min(x_axis), max(x_axis)

        return abs(min_x - max_x)
    elif rotations == 2:
        # We made two rotations: z followed by x
        # corners in counter-clockwise order
        hull = [
            (x, z)
            for (x, _, z) in [
                corners[1],
                corners[3],
                corners[2],
                corners[6],
                corners[4],
                corners[5],
                corners[1],
            ]
        ]

        # Compute area of convex polygon
        area =  sum(x * y for ((x, _), (_, y)) in zip(hull[:-1], hull[1:]))
        area -= sum(x * y for ((x, _), (_, y)) in zip(hull[1:], hull[1:]))
        # area /= 2.0
        return area


class TryNextRotation(BaseException):
    def __init__(self, angle):
        super().__init__()
        self.angle = angle


def search(target, angles, corners, rotations, depth, axis):
    if depth > 6:
        raise TryNextRotation(next(angles))

    # previous_area = shadow_area(corners, rotations)
    previous_angle = 0
    # previous_delta = abs(target - previous_area)

    for angle in angles:
        rotated_corners = rotate_points(angle, corners, axis=axis)
        area = shadow_area(rotated_corners, rotations)
        # print('[', depth,']', rotations, 'angle=', angle, 'area=', area)
        # delta = area - target

        if abs(area - target) <= 1e-6:
            return angle

        if area > target:
            # print('RECURSE', previous_angle, angle)
            return search(target, scale(previous_angle, angle), corners, rotations, depth + 1, axis)

        previous_angle = angle
        # previous_area = area
        # previous_delta = delta

    raise TryNextRotation(previous_angle)


def main():
    # target = 1.4142
    t = int(input())
    for n in range(1, t + 1):
        target = float(input())
        # print('TARGET', target)
        print('Case #{n}:'.format(
            n=n
        ))

        # Coord of the middle of each face of the cube
        faces = [
            (0.0, 0.0, 0.5),
            (0.0, 0.0, -0.5),
            (0.0, 0.5, 0.0),
            (0.0, -0.5, 0.0),
            (0.5, 0.0, 0.0),
            (-0.5, 0.0, 0.0),
        ]

        corners = [
            (x, y, z)
            for x in [0.5, -0.5]
            for y in [0.5, -0.5]
            for z in [0.5, -0.5]
        ]

        try:
            angle = search(target, scale(0.0, 46.0), corners, rotations=1, depth=0, axis=2)
            faces = rotate_points(angle, faces, axis=2)
        except TryNextRotation as ex:
            # Continue using a second rotation
            angle1 = ex.angle
            corners = rotate_points(angle1, corners, axis=2)
            faces = rotate_points(angle1, faces, axis=2)

            angle2 = search(target, scale(0.0, 46.0), corners, rotations=2, depth=0, axis=0)
            faces = rotate_points(angle2, faces, axis=0)
            # print('>>>', angle2)

        print(' '.join(map(str, faces[4])))
        print(' '.join(map(str, faces[2])))
        print(' '.join(map(str, faces[0])))


if __name__ == "__main__":
    main()

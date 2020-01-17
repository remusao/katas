#! /usr/bin/env python
# -*- coding: utf-8 -*-


from __future__ import unicode_literals, print_function
from collections import namedtuple
from itertools import islice
import numpy as np


Data = namedtuple('Data', [
    'rows',
    'slots',
    'unavailables',
    'pools',
    'servers'
])


def read_data(path):
    data = None
    with open(path, 'rb') as input_file:
        data = iter(input_file.read().split('\n'))
    R, S, U, P, M = map(int, next(data).split())
    print(R, "rows")
    print(S, "slots")
    print(U, "unavailable")
    print(P, "pools")
    print(M, "servers")

    indispo = []
    for line in islice(data, U):
        indispo.append(map(int, line.split()))

    servers = []
    for i, line in enumerate(islice(data, M)):
        size, capacity = map(int, line.split())
        servers.append((i, capacity, size))

    return Data(
        rows=R,
        slots=S,
        pools=P,
        unavailables=indispo,
        servers=servers
    )


def format_solution(path, servers, prefix):
    """ Write solution to disk. """
    with open(prefix + path, 'wb') as output:
        for server_solution in servers:
            if not np.array_equal(server_solution[2:], (-1, -1, -1)):
                print(" ".join(map(str, server_solution)), file=output)
            else:
                print("x", file=output)



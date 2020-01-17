#! /usr/bin/env python
# -*- coding: utf-8 -*-


from __future__ import print_function
import numpy as np
import random
from scipy.optimize import basinhopping
from score import score_result


class CustomStep(object):
    def __init__(self, nb_servers, nb_rows, nb_pools):
        self.nb_servers = nb_servers
        self.nb_rows = nb_rows
        self.nb_pools = nb_pools

    def __call__(self, x):
        x_shape = x.shape
        result = x.reshape((self.nb_servers, 5))

        # Random change of group
        server_id = random.randint(0, self.nb_servers - 1)
        group_id = random.randint(0, self.nb_pools)
        result[server_id, 4] = group_id
        return np.reshape(result, x_shape)


def callback(x, f, accept):
    print("callback", f)


def optimize(result, data):
    mystep = CustomStep(len(data.servers), data.rows, data.pools)
    ret = basinhopping(
        score_result,
        result,
        callback=callback,
        minimizer_kwargs={"method": "BFGS", "args": (
            len(data.servers),
            data.rows,
            data.pools)},
        niter=200,
        take_step=mystep)
    return ret.fun, ret.x

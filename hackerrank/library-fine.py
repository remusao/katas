#!/bin/python3

import sys


d2,m2,y2 = input().strip().split(' ')
d2,m2,y2 = [int(d2),int(m2),int(y2)]
d1,m1,y1 = input().strip().split(' ')
d1,m1,y1 = [int(d1),int(m1),int(y1)]

if y2 < y1: print(0)
elif y2 == y1:
    if m2 < m1: print(0)
    elif m2 == m1:
        if d2 <= d1: print(0)
        else: print(15 * abs(d1 - d2))
    else: print(500 * abs(m2 - m1))
else: print(10000)

#!/usr/bin/env python3

import fractions
import os, sys

work_path = os.path.dirname(__file__) + "/../work"
if not work_path in sys.path:
    sys.path.append(work_path)

import euler3

def is_curious(n, d):
    dn = set(euler3.digits(n))
    dd = set(euler3.digits(d))
    if len(dn) == 1 or len(dd) == 1:
        return False
    common = dn & dd
    if len(common) in (0, 2) or 0 in common:
        return False
    nc = (dn - common).pop()
    dc = (dd - common).pop()
    if dc == 0:
        return False
    f1 = fractions.Fraction(n, d)
    f2 = fractions.Fraction(nc, dc)
    return f1 == f2

np = 1
dp = 1
for d in range(10, 100):
    for n in range(10, d):
        if is_curious(n, d):
            print(n, d)
            np *= n
            dp *= d
print(fractions.Fraction(np, dp))

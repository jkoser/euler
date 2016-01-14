#!/usr/bin/env python3

import os, sys

work_path = os.path.dirname(__file__) + "/../work"
if not work_path in sys.path:
    sys.path.append(work_path)

from euler3 import primes_below

def rotations(n):
    a = n
    d = 1
    m = 1
    while a >= 10:
        d += 1
        m *= 10
        a //= 10
    for x in range(d - 1):
        n = (n % 10) * m + (n // 10)
        yield n

primes = set(primes_below(1000000))
circular = 0
for p in primes:
    if all(r in primes for r in rotations(p)):
        circular += 1
print(circular)

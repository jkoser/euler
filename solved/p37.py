#!/usr/bin/env python3

from euler3 import *

limit = 1000000
primes = set(primes_below(limit))

def is_truncatable(p):
    ds = list(digits(p))
    nd = len(ds)
    for i in range(1, nd):
        if from_digits(ds[0:i]) not in primes:
            return False
        if from_digits(ds[i:nd+1]) not in primes:
            return False
    print(p)
    return True

s = sum(p for p in primes if p >= 11 and is_truncatable(p))
print("sum:", s)

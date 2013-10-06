#!/usr/bin/env python3

from euler3 import *
from itertools import *

primes = {p for p in primes_below(10000) if p >= 1000}

#for a in primes:
#    ps = set(map(from_digits, permutations(digits(a))))
#    for d in range(1, (10000 - a) // 2 + 1):
#        b = a + d
#        c = b + d
#        if b in ps and c in ps and b in primes and c in primes:
#            print(a, b, c)

for a in primes:
    perms = set(map(from_digits, permutations(digits(a))))
    ps = sorted(p for p in perms if p in primes and p > a)
    for b in ps:
        if b + b - a in ps:
            print(a, b, b + b - a)

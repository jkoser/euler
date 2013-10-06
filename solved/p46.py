#!/usr/bin/env python3

import euler3

limit = 100000

primes = set(euler3.primes_below(limit))

def is_sum(n):
    i = 1
    x = 2
    while x < n:
        if n - x in primes:
            return True
        i += 1
        x = 2 * i * i
    return False

for n in range(9, limit, 2):
    if n not in primes and not is_sum(n):
        print(n)
        break

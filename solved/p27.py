#!/usr/bin/env python3

from euler3 import primes_below

primes = set(primes_below(10000))

# n^2 + an + b = x
# (n + 1)^2 + a(n + 1) + b
#   = n^2 + 2n + 1 + an + a + b
#   = n^2 + an + b + 2n + 1 + a
#   = x + 2n + 1 + a
def num_primes(a, b):
    n = 0
    x = b
    while x in primes:
        x += 2 * n + 1 + a
        n += 1
    return n

n_max = 0
p_max = 0
for a in range(-999, 1000):
    for b in range(-999, 1000):
        n = num_primes(a, b)
        if n > n_max:
            n_max = n
            p_max = a * b
print(n_max, p_max)

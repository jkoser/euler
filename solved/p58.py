#!/usr/bin/env python3

from euler3 import primes_below, is_prime, odd_spiral_diags

limit = 15000
side_limit = 2 * limit + 1
#primes = set(primes_below(side_limit * side_limit))
ds = list(odd_spiral_diags(limit))
t = 1
p = 0
m = 1
for r in range(0, limit):
    t += 4
    #print(ds[1+r*4:5+r*4])
    p += sum(1 for i in filter(is_prime, ds[1+r*4:5+r*4]))
    #print(r, p / t)
    if p / t < m:
        m = p / t
    if p / t < 0.1:
        print(2 * r + 3)
        break
print(m)

#!/usr/bin/env python3

# The prime 41 can be written as the sum of six consecutive primes:
#
# 41 = 2 + 3 + 5 + 7 + 11 + 13
#
# This is the longest sum of consecutive primes that adds to a prime
# below one hundred.
#
# The longest sum of consecutive primes below one thousand that adds
# to a prime contains 21 terms and is equal to 953.
#
# Which prime, below one million, can be written as the sum of the most
# consecutive primes?

from euler3 import primes_below

limit = 1000000

primes = list(primes_below(limit))
primes_set = set(primes)

m = 1
p = 0
for i in range(0, len(primes)):
    s = primes[i]
    for j in range(i + 1, len(primes)):
        s += primes[j]
        if s >= limit:
            break
        if j - i >= m and s in primes_set:
            m = j - i + 1
            p = s
print(m, p)

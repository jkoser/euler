#!/usr/bin/env python3

# Define Co(n) to be the maximal possible sum of a set of mutually co-prime
# elements from {1, 2, ..., n}.
# For example Co(10) is 30 and hits that maximum on the subset {1, 5, 7, 8, 9}.
#
# You are given that Co(30) = 193 and Co(100) = 1356.
#
# Find Co(200000).

# Co(1) = 1
# Co(2) = 1 + 2
# Co(3) = 1 + 2 + 3
# Co(4) = 1 + 4 + 3
# Co(5) = 1 + 4 + 3 + 5
# Co(6) = 1 + 4 + 3 + 5
# Co(7) = 1 + 4 + 3 + 5 + 7
# Co(8) = 1 + 8 + 3 + 5 + 7
# Co(9) = 1 + 8 + 9 + 5 + 7
# Co(10) = 1 + 8 + 9 + 5 + 7
# Co(11) = 1 + 8 + 9 + 5 + 7 + 11
# Co(29) = 1 + 4*7 + 27 + 25 + 11 + 13 + ... + 29
# Co(30) = 1 + 4*7 + 27 + 25 + 11 + 13 + ... + 29
# Co(100) = 1 + 8*11 + 81 + 5*19 + 7*13 + 17 + 23 + ... + 97

# If n is prime, Co(n) = Co(n - 1) + n
# If n is composite, max subset either uses n or doesn't.
#   If max subset does not use n, Co(n) = C(n - 1)
#   If it does, Co(n) = n + Co(n - 1, prime factors of n)

# Define Co(n, P = {p1 ... pk}) to be a solution excluding primes p1 ... pk
# As a special case, Co(n) = Co(n, {})
# If n is prime,
#   If n in P, Co(n, P) = Co(n, P \ n)
#   Else, Co(n, P) = Co(n, P) + n
# If n is composite,
#   If there is a divisor of n in P, Co(n, P) = Co(n - 1, P)
#   Else,
#     If max subset does not use n, C(n, P) = Co(n - 1, P)
#     If it does, Co(n, P) = n + C(n - 1, P + prime factors of n)

from functools import lru_cache
from euler3 import primes_below, prime_factors

primes = set(primes_below(1000000))

@lru_cache(maxsize=None)
def co(n, exclude):
    if n == 1:
        return 1
    elif any((n % p == 0) for p in exclude):
        return co(n - 1, frozenset(exclude.difference({n})))
    elif n in primes:
        return co(n - 1, exclude) + n
    else:
        r1 = co(n - 1, exclude)
        r2 = co(n - 1, frozenset(exclude.union(prime_factors(n)))) + n
        return max(r1, r2)

for i in range(1, 1001):
    print(i, co(i, frozenset()))

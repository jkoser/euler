#!/usr/bin/env python3

# The first two consecutive numbers to have two distinct prime factors are:
#
# 14 = 2 * 7
# 15 = 3 * 5
#
# The first three consecutive numbers to have three distinct prime factors
# are:
#
# 644 = 2^2 * 7 * 23
# 645 = 3 * 5 * 43
# 646 = 2 * 17 * 19
#
# Find the first four consecutive integers to have four distinct prime
# factors.  What is the first of these numbers?

from euler3 import *

n = 2
r = 0
while r < 4:
    if len(set(prime_factors(n))) == 4:
        r += 1
    else:
        r = 0
    n += 1
print(n - 4)

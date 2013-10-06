#!/usr/bin/env python3

# What is the largest n-digit pandigital prime that exists?

from euler3 import *

m = 0
for n in pandigitals(7):
    if is_prime(n) and n > m:
        m = n
print(m)

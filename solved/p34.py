#!/usr/bin/env python3

from euler3 import *

fact = [factorial(n) for n in range(0, 10)]

s = 0
for n in range(10, factorial(9) * 7):
    if (sum(fact[i] for i in digits(n))) == n:
        s += n
print(s)

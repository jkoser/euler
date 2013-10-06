#!/usr/bin/env python3

from fractions import Fraction
from euler3 import num_digits

x = Fraction(2)
c = 0
for i in range(1000):
    y = 1 + 1 / x
    if num_digits(y.numerator) > num_digits(y.denominator):
        c += 1
    x = 2 + 1 / x
print(c)

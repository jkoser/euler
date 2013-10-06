#!/usr/bin/env python3

from euler3 import continued_fraction, digit_sum

lst = [1] * 100
lst[0] = 2
for k in range(1, 34):
    i = 3 * k - 1
    lst[i] = 2 * k
f = continued_fraction(lst)
print(digit_sum(f.numerator))

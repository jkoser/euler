#!/usr/bin/env python3

# It can be seen that the number 125874 and its double 251748 contain exactly
# the same digits, but in a different order.
#
# Find the smallest positive integer x such that 2x, 3x, 4x, 5x, 6x contain
# the same digits.

from euler3 import digits

for i in range(1, 100000000):
    d = digits(i)
    d.sort()
    eq = True
    for k in range(2, 7):
        md = digits(i * k)
        md.sort()
        if md != d:
            eq = False
            break
    if eq:
        print(i)
        break

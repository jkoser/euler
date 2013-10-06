#!/usr/bin/env python3

# The number 1406357289 is a 0 to 9 pandigital number because it is made
# up of each of the digits 0 to 9 in some order, but it also has a rather
# interesting substring divisibility property.
#
# Let d1 be the 1st digit, d2 be the 2nd digit, and so on.  In this way,
# we note the following:
#
# d2d3d4 = 406 is divisible by 2
# d3d4d5 = 063 is divisible by 3
# d4d5d6 = 635 is divisible by 5
# d5d6d7 = 357 is divisible by 7
# ...
# d8d9d10 = 289 is divisible by 17
#
# Find the sum of all 0 to 9 pandigital numbers with this property.

from euler3 import *

s = 0
for n in pandigitals(10):
    if n % 1000 % 17 != 0:
        continue
    if (n // 10) % 1000 % 13 != 0:
        continue
    if (n // 100) % 1000 % 11 != 0:
        continue
    if (n // 1000) % 1000 % 7 != 0:
        continue
    if (n // 10000) % 5 != 0:
        continue
    if (n // 100000) % 1000 % 3 != 0:
        continue
    if (n // 1000000) % 2 != 0:
        continue
    print(n)
    s += n
print('sum: ', s)

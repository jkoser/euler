#!/usr/bin/env python3

# An irrational decimal fraction is created by concatenating the positive
# integers:
#
# 0.123456789101112131415...
#
# It can be seen that the 12th digit of the fractional part is 1.
#
# If dn represents the nth digit of the fractional part, find the value
# of the following expression.
#
# d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000

from euler3 import digits

n = 1
k = 1
while n <= 1000000:
    for d in digits(k):
        if n in (1, 10, 100, 1000, 10000, 100000, 1000000):
            print('d' + str(n), d)
        #else:
        #    print(d)
        n += 1
    k += 1

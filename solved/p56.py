#!/usr/bin/env python3

# A googol (10^100) is a massive number: one followed by one-hundred zeros;
# 100^100 is almost unimaginably large: one followed by two-hundred zeros.
# Despite their size, the sum of the digits in each number is only 1.

# Considering natural numbers of the form, ab, where a, b < 100, what is the
# maximum digital sum?

from euler3 import digit_sum

m = 0
for a in range(1, 101):
    n = 1
    for b in range(1, 101):
        n *= a
        ds = digit_sum(n)
        if ds > m:
            m = ds
print(m)

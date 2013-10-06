#!/usr/bin/env python3

from euler3 import combinations

c = 0
for n in range(1, 101):
    for r in range(0, n // 2 + 1):
        if combinations(n, r) > 1000000:
            c += n + 1 - 2 * r
            break
print(c)

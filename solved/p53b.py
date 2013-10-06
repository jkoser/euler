#!/usr/bin/env python3

# 7c2 = 3 * ... * 7 / (1 * ... * 5)
# 7c3 = 4 * ... * 7 / (1 * ... * 4)

c = 0
for n in range(1, 101):
    comb = 1
    for r in range(1, n // 2 + 1):
        comb *= n - r + 1
        comb //= r
        if comb > 1000000:
            c += n + 1 - 2 * r
            break
print(c)

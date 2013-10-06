#!/usr/bin/env python3

def spiral_diag_sum(n):
    r = n // 2
    s = 1
    v = 1
    for k in range(1, r + 1):
        s += 4*v + 20*k
        v += 8*k
    return s

for n in range(1, 6, 2):
    print(n, spiral_diag_sum(n))

print(1001, spiral_diag_sum(1001))

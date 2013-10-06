#!/usr/bin/env python3

import euler3

n = 0
m = 0
for k, phik in euler3.totients_below(1000000):
    r = k / phik
    if r > m:
        m = r
        n = k
print(n, m)

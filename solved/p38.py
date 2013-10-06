#!/usr/bin/env python3

from euler3 import *

def solve():
    p = 123456789
    for b in range(1, 100000):
        c = b
        k = 1
        while c < 1000000000:
            if is_pandigital(c):
                print(c, b, k)
                p = max(p, c)
                break
            k += 1
            a = b * k
            m = 1
            while m < a:
                m *= 10
            c = c * m + a
    print(p)

solve()

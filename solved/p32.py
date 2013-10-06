#!/usr/bin/env python3

from euler3 import digits

def is_pandigital(a, b, c):
    d1 = digits(a)
    d2 = digits(b)
    d3 = digits(c)
    if len(d1) + len(d2) + len(d3) != 9:
        return False
    du = set(d1) | set(d2) | set(d3)
    du.discard(0)
    return len(du) == 9

ps = set()
for a in range(1, 10000):
    for b in range(a + 1, 100000 // a):
        c = a * b
        if is_pandigital(a, b, c):
            ps.add(c)
print(sum(ps))

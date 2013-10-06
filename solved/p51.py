#!/usr/bin/env python3

from itertools import permutations
from euler3 import from_digits, is_prime

for template in set(permutations(['d', 'd', 'd', '*', '*', '*'])):
    for last in [1, 3, 7, 9]:
        t = list(template) + [last]
        for d1 in range(10):
            t1 = t[:]
            t1[t1.index('d')] = d1
            for d2 in range(10):
                t2 = t1[:]
                t2[t2.index('d')] = d2
                for d3 in range(10):
                    t3 = t2[:]
                    t3[t3.index('d')] = d3
                    c = 0
                    mm = -1
                    for m in range(10):
                        ds = [m if x == '*' else x for x in t3]
                        if is_prime(from_digits(ds)):
                            if mm < 0:
                                mm = m
                        else:
                            c += 1
                            if c > 2:
                                break
                    if c <= 2:
                        print(t3, mm)

#!/usr/bin/env python3

from itertools import permutations
from euler3 import digits, from_digits

def cubes_below(n):
    i = 1
    k = 1
    while k < n:
        yield k
        i += 1
        k = i ** 3

def cubes_by_digits():
    m = 10
    i = 1
    k = 1
    while True:
        cs = []
        while k < m:
            cs.append(k)
            i += 1
            k = i ** 3
        yield cs
        m *= 10

def digit_counts(n):
    cs = {}
    while n > 0:
        d = n % 10
        if d in cs:
            cs[d] += 1
        else:
            cs[d] = 1
        n //= 10
    return cs

def iter_over_set():
    g = cubes_by_digits()
    while True:
        cs = next(g)
        for c in cs:
            digits = digit_counts(c)
            k = 0
            for d in cs:
                if digits == digit_counts(d):
                    k += 1
            if k == 5:
                print(c)
                return

iter_over_set()

def brute_force():
    limit = 1000000000
    clist = list(cubes_below(limit))
    print(len(clist), 'cubes below', limit)
    cset = set(clist)
    for c in clist:
        ds = digits(c)
        ks = set()
        for p in permutations(ds):
            n = from_digits(p)
            if p[0] != 0 and n in cset:
                ks.add(n)
        if len(ks) == 4:
            print(c)
            break

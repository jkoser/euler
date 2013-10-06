#!/usr/bin/python

import string;

sq = [k * k for k in range(10)]

def next_in_chain(n):
    r = 0
    while n > 0:
        r += sq[n % 10]
        n = n / 10
    return r

def repeated(n):
    # print n,
    while n not in [0, 1, 89]:
        n = next_in_chain(n)
        # print n,
    # print ''
    return n

cache = [repeated(k) for k in range(700)]
count = 0
for k in range(10000000):
    if cache[next_in_chain(k)] == 89:
        count += 1
print count

#!/usr/bin/env python3

import string

words = []
with open('p42.txt', 'r') as f:
    for line in f:
        line = line.rstrip().rstrip(',')
        for s in line.split(','):
            words.append(s.strip('"'))
#print(words)

def triangles_below(n):
    i = 1
    t = 1
    while t < n:
        yield t
        i += 1
        t = i * (i + 1) // 2

triangles = set(triangles_below(1000))

offset = ord('A')
c = 0
for w in words:
    v = sum(ord(a) - offset + 1 for a in w)
    if v in triangles:
        c += 1
print(c)

#!/usr/bin/python3

import roman

saved = 0
with open('p89.txt', 'r') as f:
    for line in f:
        line = line.rstrip()
        n = roman.parse(line)
        r = roman.minimal(n)
        saved += len(line) - len(r)
print(saved)

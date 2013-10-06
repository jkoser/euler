#!/usr/bin/env python3

import fractions

s = 50

# count triangles with right angle at origin
r = s * s

# count triangles with right angle on (0, 0) - (k, k)
for k in range(s - 1, 0, -2):
    r += 2 * k

# count other triangles
for x in range(1, s + 1):
    for y in range(0, x):
        g = fractions.gcd(x, y)
        dx = x // g
        dy = y // g
        x2 = x - dy
        y2 = y + dx
        while 0 <= x2 and y2 <= s:
            r += 2
            x2 -= dy
            y2 += dx
        x2 = x + dy
        y2 = y - dx
        while x2 <= s and 0 <= y2:
            r += 2
            x2 += dy
            y2 -= dx

print(r)

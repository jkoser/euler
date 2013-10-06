#!/usr/bin/env python3

# If p is the perimeter of a right triangle with integral length sides,
# {a, b, c}, there are exactly three solutions for p = 120.
#
# {20, 48, 52}, {24, 45, 51}, {30, 40, 50}
#
# For which value of p <= 1000 is the number of solutions maximized?

def num_solutions(p):
    n = 0
    for a in range(1, p // 3 + 1):
        for b in range(a, (p - a) // 2 + 1):
            c = p - a - b
            if a * a + b * b == c * c:
                #print(a, b, c)
                n += 1
    return n

m = 0
pm = 0
for p in range(1, 1001):
    n = num_solutions(p)
    if n >= m:
        m = n
        pm = p
print(pm, m)

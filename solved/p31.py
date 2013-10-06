#!/usr/bin/env python3

denoms = [200, 100, 50, 20, 10, 5, 2, 1]

w = {}
def ways(n, di):
    if n == 0:
        return 1
    if di == len(denoms):
        return 0
    if (n, di) in w:
        return w[n, di]
    d = denoms[di]
    s = sum(ways(n - d * k, di + 1) for k in range(n // d + 1))
    w[n, di] = s
    return s

print(ways(200, 0))

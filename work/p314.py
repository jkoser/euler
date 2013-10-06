#!/usr/bin/env python3

from fractions import Fraction
from math import sqrt

ratiomax = 0
bestpath = []

def step(r, pos, slope, path, area, perim):
    global ratiomax
    global bestpath
    x, y = pos
    if y == x - 1 or (slope == 1 and (x + y) % 2 == 1):
        d = (x - y - 1) // 2
        pos1 = x1, y1 = (x - d), (y + d)
        perim1 = perim + d * sqrt(2)
        da = d * (d + 1)
        pos1 = x1, y1 = (x1 - 1), (y1 + 1)
        path1 = path + [pos1]
        perim1 += sqrt(2) / 2
        da += Fraction(1, 4)
        ratio = (area + da) / perim1
        if ratio > ratiomax:
            #print(path1, area + da, perim1)
            #print((area + da) / perim1)
            ratiomax = ratio
            bestpath = path1
        return
    for dy in range(x - y, 0, -1):
        if slope < 0:
            dxmin = 0
        else:
            dxmin = int(slope * dy) + 1
        for dx in range(dxmin, min(dy, x - y - dy) + 1):
            da = dy * (Fraction(1, 2) * dy + (x - y - dy) - Fraction(1, 2) * dx)
            x1, y1 = (x - dx, y + dy)
            pos1 = x1, y1
            path1 = path + [pos1]
            perim1 = perim + sqrt(dx * dx + dy * dy)
            if x1 == y1:
                ratio = (area + da) / perim1
                if ratio > ratiomax:
                    #print(path1, area + da, perim1)
                    #print((area + da) / perim1)
                    ratiomax = ratio
                    bestpath = path1
            else:
                slope1 = dx / dy
                step(r, pos1, slope1, path1, area + da, perim1)


def show_path(r, path):
    for y in range(r, -1, -1):
        for x in range(0, r + 1):
            if (x, y) in path or (y, x) in path:
                print(' o', end='')
            else:
                print(' .', end='')
        print()

def p314(r, showpath=True):
    global ratiomax
    global bestpath
    pos = (r, 0)
    path = [pos]
    ratiomax = 0
    step(r, pos, -1, path, 0, 0)
    print(bestpath)
    print(ratiomax)
    if showpath:
        show_path(r, bestpath)

def p314_dp(r, showpath=True):
    # (area, pathlen, path) tuple matrix
    m = [[]] * (r + 1)
    for y in range(r, -1, -1):
        m[y] = [None] * (r + 1)
        m[y][y] = (Fraction(0, 1), 0, [(y, y)])
        if y < r:
            x0, y0 = y + Fraction(1, 2), y + Fraction(1, 2)
            a = (y + 1) * y0 - Fraction(1, 8) - Fraction(y0 * x0, 2) - Fraction((y + 1) * y, 2)
            m[y+1][y] = (a, sqrt(2) / 2, [(y, y+1), (y+1, y)])
        for x in range(y + 2, r + 1):
            best_ratio = 0
            best_area = 0
            best_perim = 0
            best_path = []
            for dy in range(x - y, 0, -1):
                for dx in range(0, min(dy, x - y - dy) + 1):
                    x0, y0 = x - dx, y + dy
                    a0, p0, path0 = m[x0][y0]
                    a = a0 + x * y0 - Fraction(dx * dy, 2) - Fraction(y0 * x0, 2) - Fraction(x * y, 2)
                    p = p0 + sqrt(dx * dx + dy * dy)
                    ratio = a / p
                    if ratio > best_ratio:
                        best_ratio = ratio
                        best_area = a
                        best_perim = p
                        best_path = path0 + [(x, y)]
            m[x][y] = (best_area, best_perim, best_path)
    best_ratio = 0
    best_path = 0
    for x in range(1, r + 1):
        a, p, path = m[x][0]
        ratio = a / p
        if ratio > best_ratio:
            best_ratio = ratio
            best_path = path
    print(best_path)
    print(best_ratio)
    if showpath:
        show_path(r, best_path)

for r in range(3, 50):
    p314(r, False)

# show_path(250, [(214, 208), (220, 201), (226, 193), (230, 187), (234, 180), (238, 172), (242, 162), (244, 156), (246, 149), (247, 145), (248, 140), (249, 133), (250, 122)])
# p314(23, False)
#for r in range(6, 11):
#    print('--- r = ' + str(r) + ' ---')
#    p314(r, True)
#    p314_dp(r, True)

#!/usr/bin/env python3

from math import sqrt
import random
from ga import ga

r = 250
k = 20

def ga_generator():
    ys = random.sample(range(1, r + 1), k)
    ys.sort(reverse=True)
    x = random.randint(ys[0], r)
    c = [(x, ys[0])]
    for y in ys[1:]:
        if x == r:
            break
        x = random.randint(x + 1, r)
        c.append((x, y))
    return c

# Best:  [(222, 200), (229, 190), (237, 177), (242, 166), (247, 148), (249, 139), (250, 126)]
# A / P = 132.4747995621209
# Best:  [(221, 201), (228, 191), (240, 169), (245, 154), (247, 146), (248, 140), (249, 133), (250, 119)]
# A / P = 132.4824833963893
# Best:  [(221, 200), (228, 190), (234, 180), (240, 167), (244, 157), (245, 154), (247, 146), (249, 136), (250, 125)]
# A / P = 132.49593267194197
# Best:  [(219, 204), (228, 192), (237, 176), (242, 164), (246, 152), (247, 148), (249, 137), (250, 127)]
# Best A / P = 132.50304728400914
# Best:  [(218, 205), (228, 192), (236, 178), (242, 164), (246, 151), (248, 142), (249, 136), (250, 125)]
# Best A / P = 132.50747169698715
# Best:  [(217, 206), (222, 200), (229, 190), (236, 178), (242, 164), (246, 151), (247, 147), (249, 136), (250, 125)]
# Best A / P = 132.51385841431096
# Best:  [(217, 206), (222, 200), (229, 190), (236, 178), (238, 174), (243, 162), (246, 152), (248, 143), (249, 136), (250, 125)]
# Best A / P = 132.51690240645053
# Best:  [(216, 206), (222, 199), (228, 191), (233, 183), (237, 175), (241, 166), (243, 161), (246, 151), (248, 142), (249, 135), (250, 123)]
# Best A / P = 132.5203420441363
# Best:  [(216, 206), (222, 199), (227, 192), (232, 184), (237, 175), (242, 163), (244, 157), (246, 150), (248, 141), (249, 134), (250, 123)]
# Best A / P = 132.52201485778994
# Best:  [(215, 207), (221, 200), (227, 192), (232, 184), (237, 175), (240, 168), (243, 160), (246, 150), (248, 141), (249, 134), (250, 123)]
# Best A / P = 132.52336428516045
# Best:  [(215, 207), (221, 200), (227, 192), (232, 184), (236, 177), (238, 173), (242, 163), (244, 157), (246, 150), (248, 141), (249, 134), (250, 123)]
# Best A / P = 132.5242116544609
# Best:  [(215, 208), (221, 201), (224, 197), (229, 190), (232, 185), (236, 178), (238, 174), (242, 164), (244, 158), (246, 151), (248, 142), (249, 135), (250, 124)]
# Best A / P = 132.52433719166712
# Best:  [(215, 207), (221, 200), (227, 192), (231, 186), (236, 177), (238, 173), (242, 163), (244, 157), (246, 150), (248, 141), (249, 134), (250, 123)]
# Best A / P = 132.5247050324143
# Best:  [(215, 207), (221, 200), (227, 192), (229, 189), (232, 184), (236, 177), (238, 173), (242, 163), (244, 157), (246, 150), (248, 141), (249, 134), (250, 123)]
# Best A / P = 132.5249598498387
# Best:  [(215, 207), (221, 200), (227, 192), (231, 186), (235, 179), (236, 177), (238, 173), (241, 166), (244, 157), (246, 150), (248, 141), (249, 134), (250, 123)]
# Best A / P = 132.5252444622543
# Best:  [(214, 208), (220, 201), (226, 193), (230, 187), (234, 180), (238, 172), (242, 162), (244, 156), (246, 149), (247, 145), (248, 140), (249, 133), (250, 122)]
# Best A / P = 132.52655447986328

def ga_generator2():
    r = random.random()
    if r < 0.005:
        return [(214, 208), (220, 201), (226, 193), (230, 187), (234, 180), (238, 172), (242, 162), (244, 156), (246, 149), (247, 145), (248, 140), (249, 133), (250, 122)]
    elif r < 0.01:
        return [(215, 207), (221, 200), (227, 192), (231, 186), (235, 179), (236, 177), (238, 173), (241, 166), (244, 157), (246, 150), (248, 141), (249, 134), (250, 123)]
    elif r < 0.03:
        return [(random.randint(217, 223), random.randint(199, 206)), \
                (random.randint(227, 230), random.randint(189, 193)), \
                (random.randint(233, 241), random.randint(168, 180)), \
                (random.randint(239, 246), random.randint(154, 167)), \
                (random.randint(243, 248), random.randint(149, 153)), \
                (random.randint(247, 249), random.randint(138, 148)), \
                (random.randint(250, 250), random.randint(118, 128))]
    elif r < 0.06:
        return [(random.randint(215, 220), random.randint(204, 210)), \
                (random.randint(221, 225), random.randint(194, 203)), \
                (random.randint(227, 230), random.randint(189, 193)), \
                (random.randint(233, 241), random.randint(168, 180)), \
                (random.randint(239, 246), random.randint(154, 167)), \
                (random.randint(243, 248), random.randint(149, 153)), \
                (random.randint(248, 249), random.randint(134, 144)), \
                (random.randint(250, 250), random.randint(118, 128))]
    elif r < 0.09:
        return [(random.randint(215, 218), random.randint(204, 208)), \
                (random.randint(221, 225), random.randint(198, 203)), \
                (random.randint(227, 230), random.randint(189, 193)), \
                (random.randint(235, 237), random.randint(176, 182)), \
                (random.randint(237, 239), random.randint(170, 175)), \
                (random.randint(239, 245), random.randint(157, 167)), \
                (random.randint(245, 247), random.randint(149, 154)), \
                (random.randint(248, 248), random.randint(140, 145)), \
                (random.randint(249, 249), random.randint(134, 139)), \
                (random.randint(250, 250), random.randint(118, 130))]
    elif r < 0.12:
        return [(random.randint(215, 217), random.randint(205, 207)), \
                (random.randint(221, 223), random.randint(198, 201)), \
                (random.randint(227, 229), random.randint(190, 193)), \
                (random.randint(231, 234), random.randint(181, 185)), \
                (random.randint(236, 238), random.randint(172, 177)), \
                (random.randint(240, 242), random.randint(162, 167)), \
                (random.randint(243, 245), random.randint(156, 162)), \
                (random.randint(245, 247), random.randint(149, 153)), \
                (random.randint(248, 248), random.randint(140, 145)), \
                (random.randint(249, 249), random.randint(132, 137)), \
                (random.randint(250, 250), random.randint(120, 126))]
    elif r < 0.15:
        return [(random.randint(215, 216), random.randint(205, 207)), \
                (random.randint(220, 222), random.randint(198, 201)), \
                (random.randint(227, 229), random.randint(190, 194)), \
                (random.randint(231, 233), random.randint(181, 185)), \
                (random.randint(235, 236), random.randint(175, 178)), \
                (random.randint(237, 238), random.randint(172, 174)), \
                (random.randint(240, 242), random.randint(162, 167)), \
                (random.randint(243, 245), random.randint(156, 160)), \
                (random.randint(246, 247), random.randint(149, 153)), \
                (random.randint(248, 248), random.randint(140, 144)), \
                (random.randint(249, 249), random.randint(132, 137)), \
                (random.randint(250, 250), random.randint(120, 126))]
    elif r < 0.18:
        return [(random.randint(215, 216), random.randint(205, 209)), \
                (random.randint(220, 222), random.randint(199, 203)), \
                (random.randint(223, 225), random.randint(195, 198)), \
                (random.randint(227, 229), random.randint(188, 194)), \
                (random.randint(231, 233), random.randint(181, 187)), \
                (random.randint(235, 236), random.randint(176, 179)), \
                (random.randint(237, 238), random.randint(172, 175)), \
                (random.randint(240, 242), random.randint(162, 167)), \
                (random.randint(243, 245), random.randint(156, 160)), \
                (random.randint(246, 247), random.randint(149, 153)), \
                (random.randint(248, 248), random.randint(140, 144)), \
                (random.randint(249, 249), random.randint(132, 137)), \
                (random.randint(250, 250), random.randint(122, 126))]
    elif r < 0.21:
        return [(random.randint(215, 215), random.randint(206, 208)), \
                (random.randint(220, 222), random.randint(199, 202)), \
                (random.randint(224, 228), random.randint(190, 196)), \
                (random.randint(229, 230), random.randint(188, 189)), \
                (random.randint(231, 233), random.randint(181, 187)), \
                (random.randint(235, 236), random.randint(176, 179)), \
                (random.randint(237, 238), random.randint(172, 175)), \
                (random.randint(240, 242), random.randint(162, 167)), \
                (random.randint(243, 245), random.randint(156, 160)), \
                (random.randint(246, 247), random.randint(149, 153)), \
                (random.randint(248, 248), random.randint(140, 144)), \
                (random.randint(249, 249), random.randint(132, 137)), \
                (random.randint(250, 250), random.randint(122, 126))]
    else:
        return ga_generator()

def ga_generator3():
    r = random.random()
    if r < 0.01:
        return [(214, 208), (220, 201), (226, 193), (230, 187), (234, 180), (238, 172), (242, 162), (244, 156), (246, 149), (247, 145), (248, 140), (249, 133), (250, 122)]
    elif r < 0.5:
        c = [(214, 208), (220, 201), (226, 193), (230, 187), (234, 180), (238, 172), (242, 162), (244, 156), (246, 149), (247, 145), (248, 140), (249, 133), (250, 122)]
        n = len(c)
        i = random.randrange(n)
        dx = random.randint(-1, 1)
        dy = random.randint(-1, 1)
        x, y = c[i]
        c[i] = x + dx, y + dy
        return c
    else:
        return [(random.randint(215, 215), random.randint(206, 208)), \
                (random.randint(220, 222), random.randint(199, 202)), \
                (random.randint(224, 228), random.randint(190, 196)), \
                (random.randint(229, 230), random.randint(188, 189)), \
                (random.randint(231, 233), random.randint(181, 187)), \
                (random.randint(235, 236), random.randint(176, 179)), \
                (random.randint(237, 238), random.randint(172, 175)), \
                (random.randint(240, 242), random.randint(162, 167)), \
                (random.randint(243, 245), random.randint(156, 160)), \
                (random.randint(246, 247), random.randint(149, 153)), \
                (random.randint(248, 248), random.randint(140, 144)), \
                (random.randint(249, 249), random.randint(132, 137)), \
                (random.randint(250, 250), random.randint(122, 126))]

def ap_ratio(c):
    n = len(c)
    if n == 0:
        return 0
    x0, y0 = c[0]
    if x0 < y0 or y0 > r:
        return 0
    a = (x0 ** 2 - (x0 - y0) ** 2 / 2 - x0 * y0) / 2
    p = sqrt(2) * (x0 - y0) / 2
    for i in range(n - 1):
        x0, y0 = c[i]
        x1, y1 = c[i + 1]
        if x0 >= x1 or y1 >= y0:
            return 0
        a += y0 * x1 - x0 * y0 / 2 - (y0 - y1) * (x1 - x0) / 2 - x1 * y1 / 2
        p += sqrt((y0 - y1) ** 2 + (x1 - x0) ** 2)
    x0, y0 = c[n - 1]
    if x0 > r:
        return 0
    a += y0 * r / 2
    p += sqrt(y0 ** 2 + (r - x0) ** 2)
    return a / p

def ga_fitness(c):
    ap = ap_ratio(c)
    if ap == 0:
        return 0
    else:
        return 1.10 ** (ap - r / 2)

def ga_crossover(c1, c2):
    yp = random.randint(1, r - 1)
    cnew = [p for p in c1 if p[1] > yp] + [p for p in c2 if p[1] <= yp]
    if cnew != []:
        return cnew
    else:
        return c1

def ga_mutation(c):
    n = len(c)
    cnew = c[:]
    if random.random() < 0.02 and n > 1:
        i = random.randrange(n)
        cnew[i:i+1] = []
        n -= 1
    for m in range(random.randint(1, 5)):
        i = random.randrange(n)
        x, y = cnew[i]
        d = random.randrange(2)
        if d == 0:
            xmin = y
            if i > 0:
                xmin = cnew[i - 1][0] + 1
            xmax = r
            if i < n - 1:
                xmax = cnew[i + 1][0] - 1
            if xmin > xmax:
                return cnew
            else:
                x = random.randint(xmin, xmax)
        else:
            ymin = 1
            if i < n - 1:
                ymin = cnew[i + 1][1] + 1
            ymax = x
            if i > 0:
                ymax = cnew[i - 1][1] - 1
            y = random.randint(max(ymin, y - 1), min(ymax, y + 1))
        cnew[i] = x, y
    return cnew

def main():
    best_ap = 0
    best_c = None
    for i in range(1):
        c = ga(ga_generator3, ga_fitness, ga_crossover, ga_mutation, \
                20000, 5000, 10, 10, 0.5, 20, True)
        ap = ap_ratio(c)
        if ap > best_ap:
            best_ap = ap
            best_c = c
        print("Best A / P =", best_ap)
    print("Best: ", best_c)
    print("Best A / P =", best_ap)

# main()

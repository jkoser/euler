#!/usr/bin/env python3

from fractions import Fraction

def is_right_angle(p1, p2, p3):
    dx1 = p1[0] - p2[0]
    dy1 = p1[1] - p2[1]
    dx2 = p3[0] - p2[0]
    dy2 = p3[1] - p2[1]
    if dx1 == 0 and dy2 == 0 or dx2 == 0 and dy1 == 0:
        return True
    if dx1 == 0 or dy1 == 0 or dx2 == 0 or dy2 == 0:
        return False
    if Fraction(dy1, dx1) == - Fraction(dx2, dy2):
        return True
    return False

def forms_right_triangle(p1, p2, p3):
    if p1 == p2 or p1 == p3 or p2 == p3:
        return False
    if is_right_angle(p1, p2, p3):
        return True
    if is_right_angle(p1, p3, p2):
        return True
    if is_right_angle(p2, p1, p3):
        return True
    return False

limit = 31

c = 0
for x1 in range(0, limit):
    for y1 in range(0, limit):
        for x2 in range(0, limit):
            for y2 in range(0, limit):
                if forms_right_triangle((0, 0), (x1, y1), (x2, y2)):
                    c += 1
print(c // 2)

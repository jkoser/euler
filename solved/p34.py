#!/usr/bin/env python3

import os, sys

work_path = os.path.dirname(__file__) + "/../work"
if not work_path in sys.path:
    sys.path.append(work_path)

from euler3 import *

fact = [factorial(n) for n in range(0, 10)]

def eq_fact_of_digits(n):
    s = 0
    m = n
    while m > 0:
        s += fact[m % 10]
        m //= 10
    return s == n

s = 0
for n in range(10, fact[9] * 6):
    if eq_fact_of_digits(n):
        print(n)
        s += n
print("sum:", s)

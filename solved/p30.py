#!/usr/bin/env python3

def is_sum_fifth(n):
    m = n
    s = 0
    while m > 0:
        s += (m % 10) ** 5
        m = m // 10
    return s == n

print(sum(k for k in range(10, 354294) if is_sum_fifth(k)))

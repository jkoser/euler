#!/usr/bin/env python3

from euler3 import reverse_digits, is_palindrome

c = 0
for n in range(1, 10001):
    lychrel = True
    for k in range(1, 51):
        n = n + reverse_digits(n)
        if is_palindrome(n):
            lychrel = False
            break
    if lychrel:
        c += 1
print(c)

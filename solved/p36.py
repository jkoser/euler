#!/usr/bin/env python3

from euler3 import is_palindrome

s = 0
for k in range(1, 1000000):
    if is_palindrome(k) and is_palindrome(k,2):
        s += k
print(s)

#!/usr/bin/env python3

# The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit
# number, 134217728=8^9, is a ninth power.
# How many n-digit positive integers exist which are also an nth power?

# 1^1, 2^1, 3^1, 4^1... 9^1
# 4^2, 5^2, 6^2, 7^2, 8^2, 9^2
# 5^3... 9^3
# 6^4... 9^4
# 7^5... 9^5
# 7^6... 9^6
# 8^7, 9^7
# 8^8

c = 0
found = True
p = 1
while found:
    found = False
    low = 10 ** (p - 1)
    high = 10 ** p
    for b in range(1, 10):
        n = b ** p
        if n >= low and n < high:
            c += 1
            found = True
    p += 1
print(c)

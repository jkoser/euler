#!/usr/bin/env python3

limit = 28124

def proper_divisors(n):
    if n > 1:
        yield 1
    i = 2
    while i * i <= n:
        if n % i == 0:
            yield i
            j = n // i
            if j != i:
                yield j
        i += 1

abundant = [k for k in range(12, limit) if sum(proper_divisors(k)) > k]
abundant_sums = {a + b for a in abundant for b in abundant if a <= b}

result = 0
for n in range(limit):
    if n not in abundant_sums:
        result += n

print(result)

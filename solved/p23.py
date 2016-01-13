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

#abundant = [k for k in range(12, limit) if sum(proper_divisors(k)) > k]
#abundant_sums = {a + b for a in abundant for b in abundant if a <= b}

divisor_sums = [1] * limit
for i in range(2, limit):
    for j in range(i * 2, limit, i):
        divisor_sums[j] += i
abundant = {i for i in range(1, limit) if divisor_sums[i] > i}
print(len(abundant), "abundant numbers below", limit)

result = 0
for n in range(limit):
    ab_sum = False
    for a in abundant:
        if n - a in abundant:
            ab_sum = True
            break
    if not ab_sum:
        result += n

print(result)

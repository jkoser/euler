#!/usr/bin/python

def primes_below(n):
    sieve = [True for k in range(n)]
    for i in range(2, n):
        if sieve[i]:
            yield i
            j = i * 2
            while j < n:
                sieve[j] = False
                j += i

def rotations(n):
    a = n
    d = 1
    m = 1
    while a >= 10:
        d += 1
        m *= 10
        a /= 10
    for x in range(d - 1):
        n = (n % 10) * m + (n / 10)
        yield n

primes = set(primes_below(1000000))
circular = 0
for p in primes:
    if all(r in primes for r in rotations(p)):
        circular += 1
print circular

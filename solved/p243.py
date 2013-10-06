#!/usr/bin/env python3

import cProfile
from fractions import Fraction
from euler3 import primes_below, phi

def main(limit):
    primes = list(primes_below(limit))
    goal = Fraction(15499, 94744)
    phi = 1
    n = 1
    for p in primes:
        for k in range(2, p):
            print(n * k, (phi * k) / (n * k - 1))
            r = Fraction(phi * k, n * k - 1)
            if r < goal:
                print(n * k)
                return
        phi *= p - 1
        n *= p
        print(n, phi / (n - 1))
        if Fraction(phi, n - 1) < goal:
            print(n)
            return

main(100)
#cProfile.run('main(10000)')

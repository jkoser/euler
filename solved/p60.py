#!/usr/bin/env python3

from euler3 import primes_below, is_prime, cat_digits
from heapq import heappush, heappop

primes = []
primes_set = set()
primes_dict = {}

def init():
    global primes, primes_set
    primes = list(primes_below(10000000))
    primes_set = set(primes)

hits = 0
calls = 0

def is_prime2(n):
    global primes_set, primes_dict, hits, calls
    #calls += 1
    #if calls % 10000 == 0:
    #    print(hits, calls, hits/calls)
    if n < 10000000:
        return n in primes_set
    #elif n in primes_dict:
    #    hits += 1
    #    return primes_dict[n]
    else:
        p = is_prime(n)
    #    primes_dict[n] = p
        return p

def new_element_p(x, xs):
    for y in xs:
        if not is_prime(cat_digits(x, y)):
            return False
        if not is_prime(cat_digits(y, x)):
            return False
    return True
    #return all((is_prime2(cat_digits(x, y)) and is_prime2(cat_digits(y, x))) for y in xs)

# f generates integers
# g generates lists of integers
# return values are (int, list) tuples
def merge_generators_sorted(f, g):
    g1 = next(g)
    g1sum = sum(g1)
    gs = [g1]
    f1 = 0
    while f1 <= g1[0]:
        f1 = next(f)
    fs = [f1]
    q = []
    first = True
    yield (f1, g1)
    heappush(q, (f1 + sum(g1), f1, g1))
    while len(q) > 0:
        s = q[0][0]
        while s >= fs[-1] + g1sum:
            i = next(f)
            fs.append(i)
            for j in gs:
                if i > j[0]:
                    heappush(q, (i + sum(j), i, j))
        while s >= f1 + sum(gs[-1]):
            j = next(g)
            gs.append(j)
            for i in fs:
                if i > j[0]:
                    heappush(q, (i + sum(j), i, j))
        r = heappop(q)
        if not first:
            yield (r[1], r[2])
        else:
            first = False

def prime_sets_by_sum(k):
    if k == 1:
        for p in primes:
            yield [p]
    else:
        g = prime_sets_by_sum(k - 1)
        h = merge_generators_sorted(iter(primes), g)
        for r in h:
            if new_element_p(r[0], r[1]):
                ps = [r[0]] + r[1]
                if k == 4:
                    print(ps, sum(ps))
                yield ps

def main():
    init()
    for s in prime_sets_by_sum(5):
        print(s)
        print(sum(s))
        break

def main2():
    #init()
    ps = list(primes_below(20000))
    for p1 in ps:
        s1 = [p1]
        for p2 in ps:
            if p2 < p1:
                continue
            if not new_element_p(p2, s1):
                continue
            s2 = [p2] + s1
            for p3 in ps:
                if p3 < p2:
                    continue
                if not new_element_p(p3, s2):
                    continue
                s3 = [p3] + s2
                for p4 in ps:
                    if p4 < p3:
                        continue
                    if not new_element_p(p4, s3):
                        continue
                    s4 = [p4] + s3
                    for p5 in ps:
                        if p5 < p4:
                            continue
                        if not new_element_p(p5, s4):
                            continue
                        print([p5] + s4)
                        print(p5 + sum(s4))

main2()
#import cProfile
#cProfile.run('main()')

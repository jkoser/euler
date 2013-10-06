from fractions import Fraction
from math import sqrt

from miller_rabin import miller_rabin


def polygonals_below(n, r):
    i = 1
    p = 1
    while p < n:
        yield p
        i += 1
        p = i * ((r - 2) * i - (r - 4)) // 2

def primes_below(n):
    if n <= 2:
        return
    yield 2
    composite = bytearray(n)
    for i in range(3, n, 2):
        if composite[i] == 0:
            yield i
            j = i * 3
            while j < n:
                composite[j] = 1
                j += i * 2

# yields (k, phi(k)) tuples from k = 2
def totients_below(n):
    if n <= 2:
        return
    totient = [1] * n
    for i in range(2, n):
        if totient[i] == 1:
            # prime
            # multiples of prime
            j = i
            while j < n:
                totient[j] *= i - 1
                j += i
            # multiples of powers of prime
            ix = i * i
            while ix < n:
                j = ix
                while j < n:
                    totient[j] *= i
                    j += ix
                ix *= i
        else:
            # composite
            pass
        yield (i, totient[i])

def is_prime(n):
    if n < 2:
        return False
    elif n <= 3:
        return True
    elif n % 2 == 0 or n % 3 == 0:
        return False
    else:
        return miller_rabin(n)

def prime_factors(n):
    if n < 2:
        return
    i = 2
    while i * i <= n:
        if n % i == 0:
            yield i
            n //= i
        else:
            i += 1
    if n > 1:
        yield n

def phi(n):
    if n <= 2:
        return 1
    prod = 1
    last = 0
    i = 2
    while i * i <= n:
        if n % i == 0:
            if i == last:
                prod *= i
            else:
                prod *= i - 1
                last = i
            n //= i
        else:
            i += 1
    if n > 1:
        if n == last:
            prod *= n
        else:
            prod *= n - 1
    return prod

def digits(n, base=10):
    if n == 0:
        return [0]
    ds = []
    while n > 0:
        ds.append(n % base)
        n //= base
    ds.reverse()
    return ds

def from_digits(ds, base=10):
    s = 0
    for d in ds:
        s = s * base + d
    return s

def reverse_digits(n, base=10):
    if n < base:
        return n
    r = 0
    while n > 0:
        r = r * base + n % base
        n //= base
    return r

def digit_sum(n, base=10):
    s = 0
    while n > 0:
        s += n % base
        n //= base
    return s

def num_digits(n, base=10):
    if n < base:
        return 1
    d = 0
    while n > 0:
        d += 1
        n //= base
    return d

def cat_digits(n, m):
    return int(str(n) + str(m))

def pandigitals(k=9):
    used = [False for i in range(k + 1)]
    depth = 1
    used[1] = True
    n = 1
    goingin = True
    while depth > 0:
        #print(used, n, depth)
        if depth == k:
            #print(n)
            yield n
            depth -= 1
            used[n % 10] = False
            n //= 10
            goingin = False
        elif goingin:
            i = 1
            while i <= k:
                if not used[i % 10]:
                    break
                i += 1
            assert i <= k
            depth += 1
            used[i % 10] = True
            n = n * 10 + i % 10
        else:
            i = n % 10 + 1
            if i == 1:
                i = 11
            while i <= k:
                if not used[i % 10]:
                    break
                i += 1
            if depth == 1 and i == 10:
                return
            if i <= k:
                used[n % 10] = False
                used[i % 10] = True
                n //= 10
                n = n * 10 + i % 10
                goingin = True
            else:
                depth -= 1
                used[n % 10] = False
                n //= 10

def is_pandigital(n, k=9):
    b = [False for i in range(10)]
    nd = 0
    while n > 0:
        d = n % 10
        if b[d] == True or d == 0 or d > k:
            return False
        b[d] = True
        nd += 1
        n //= 10
    return nd == k

def is_pandigital_0(n):
    if n < 1000000000:
        return False
    b = [False for i in range(10)]
    while n > 0:
        d = n % 10
        if b[d] == True:
            return False
        b[d] = True
        n //= 10
    return True

def is_palindrome(n, base=10):
    if n % base == 0:
        return False
    e = 0
    while e < n:
        e = e * base + n % base
        if e == n:
            break
        n //= base
    return e == n

def factorial(n):
    p = 1
    for k in range(2, n + 1):
        p *= k
    return p

def combinations(n, r):
    #return factorial(n) / factorial(r) / factorial(n - r)
    c = 1
    for k in range(r + 1, n + 1):
        c *= k
    for k in range(2, n - r + 1):
        c //= k
    return c

def continued_fraction(lst):
    lst.reverse()
    x = Fraction(lst[0])
    for k in lst[1:]:
        x = k + 1 / x
    return x

def odd_spiral_diags(r):
    yield 1
    m = 1
    for i in range(1, r + 1):
        for j in range(4):
            m += 2 * i
            yield m

def even_spiral_diags(r):
    m = 0
    for i in range(0, r):
        for j in range(4):
            m += 2 * i + 1
            yield m

def polygon_area(p):
    return 0.5 * abs(sum(x0*y1 - x1*y0
                         for ((x0, y0), (x1, y1)) in polygon_segments(p)))

def polygon_segments(p):
    return zip(p, p[1:] + [p[0]])

def segment_length(s):
    return sqrt((s[0][0] - s[1][0]) ** 2 + (s[0][1] - s[1][1]) ** 2)
    
def polygon_perimeter(p):
    return sum(segment_length(s) for s in polygon_segments(p))

if __name__ == "__main__":
    polygons = [[(10, 0), (10, 7), (7, 10), 
                 (0, 10), (-7, 10), (-10, 7),
                 (-10, 0), (-10, -7), (-7, -10),
                 (0, -10), (7, -10), (10, -7)],
                [(10, 0), (10, 8), (7, 10), 
                 (0, 10), (-8, 10), (-10, 7),
                 (-10, 0), (-10, -8), (-7, -10),
                 (0, -10), (8, -10), (10, -7)],
                [(10, 0), (10, 8), (6, 10), 
                 (0, 10), (-8, 10), (-10, 6),
                 (-10, 0), (-10, -8), (-6, -10),
                 (0, -10), (8, -10), (10, -6)],
                [(10, 0), (10, 6), (6, 10), 
                 (0, 10), (-6, 10), (-10, 6),
                 (-10, 0), (-10, -6), (-6, -10),
                 (0, -10), (6, -10), (10, -6)],
                [(10, 0), (10, 7), (6, 10), 
                 (0, 10), (-7, 10), (-10, 6),
                 (-10, 0), (-10, -7), (-6, -10),
                 (0, -10), (7, -10), (10, -6)],
                [(10, 0), (10, 5), (5, 10), 
                 (0, 10), (-5, 10), (-10, 5),
                 (-10, 0), (-10, -5), (-5, -10),
                 (0, -10), (5, -10), (10, -5)],
                [(10, 0), (10, 5), (8, 8), (5, 10), 
                 (0, 10), (-5, 10), (-8, 8), (-10, 5),
                 (-10, 0), (-10, -5), (-8, -8), (-5, -10),
                 (0, -10), (5, -10), (8, -8), (10, -5)]]
    for polygon in polygons:
        print(polygon)
        a = polygon_area(polygon)
        p = polygon_perimeter(polygon)
        print("area =", a, ", perimeter =", p, ", a/p =", a/p)
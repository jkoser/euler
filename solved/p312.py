#!/usr/bin/env python3

# A Sierpinski graph of order 1 (S1) is an equilateral triangle.
#
# Sn+1 is obtained from Sn by positioning three copies of Sn so that
# every pair of copies has one common corner.
#
# Let C(n) be the number of cycles that pass exactly once through
# all the vertices of Sn.
#
# C(1) = C(2) = 1
# C(3) = 8
# C(5) = 71328803586048
# C(10000) mod 10^8 = 37652224
# C(10000) mod 13^8 = 617720485
#
# Find C(C(C(10000))) mod 13^8.

# Let P(n) be the number of paths that begin at one given corner and
# end at another given corner and pass exactly once through all
# vertices of Sn.
#
# P(1) = 1
# P(2) = 2
#
# Since a cycle in Sn+1 must pass through each common corner of the Sn
# copies once, we have:
#
# C(n+1) = P(n) ^ 3
# C(3) = P(2) ^ 3 = 2 ^ 3 = 8
#
# Let T(n) be the number of paths like those in P(n), but passing
# through all vertices *except* the third corner.
#
# T(1) = 1
# T(2) = 3
#
# P(n+1) = 2 * P(n) * P(n) * T(n)
# T(n+1) = 2 * P(n) * T(n) * T(n)
#
# n     T(n)    P(n)    C(n)
# 1     1       1       1
# 2     3       2       1
#       (0,1)   (1,0)
# 3     36      24      8
#       (2,2)   (3,1)
# 4     62208   41472   13824
#       (8,5)   (9,4)
# 5     ...     ...     71328803586048
#       (26,14) (27,13)
#
# From the recurrences for P and T we know that every value can be
# expressed as 2^x * 3^y.  We consider recurrences of the exponents:
#
# T2E(2) = 0
# P2E(2) = 1
# T2E(n+1) = 1 + 2 * T2E(n) + P2E(n)
# P2E(n+1) = 1 + T2E(n) + 2 * P2E(n)
# T3E(2) = 1
# P3E(2) = 0
# T3E(n+1) = 2 * T3E(n) + P3E(n)
# P3E(n+1) = T3E(n) + 2 * P3E(n)
#
# And their solutions:
#
# T2E(n) = 3 ^ (n-2) - 1
# P2E(n) = 3 ^ (n-2)
#
# T2E(n+1) = 1 + 2 * (3 ^ (n-2) - 1) + 3 ^ (n-2)
#          = 1 + 2 * 3 ^ (n-2) - 2 + 3 ^ (n-2)
#          = 3 ^ (n-1) - 1
# P2E(n+1) = 1 + 3 ^ (n-2) - 1 + 2 * 3 ^ (n-2)
#          = 3 ^ (n-1)
#
# T3E(n) = (3 ^ (n-2) + 1) / 2
# P3E(n) = (3 ^ (n-2) - 1) / 2
#
# T3E(n+1) = 2 * (3 ^ (n-2) + 1) / 2 + (3 ^ (n-2) - 1) / 2
#          = (2 * 3 ^ (n-2) + 2 + 3 ^ (n-2) - 1) / 2
#          = (3 ^ (n-1) + 1) / 2
# P3E(n+1) = (3 ^ (n-2) + 1) / 2 + 2 * (3 ^ (n-2) - 1) / 2
#          = (3 ^ (n-2) + 1 + 2 * 3 ^ (n-2) - 2) / 2
#          = (3 ^ (n-1) - 1) / 2
#
# Finally, for n >= 3, we have:
#
# C(n) = P(n-1) ^ 3
#      = (2 ^ P2E(n-1) * 3 ^ P3E(n-1)) ^ 3
#      = (2 ^ (3 ^ (n-3)) * 3 ^ ((3 ^ (n-3) - 1) / 2)) ^ 3
#      = 2 ^ (3 ^ (n-2)) * 3 ^ (3 * (3 ^ (n-3) - 1) / 2)

def cycles(n, m):
    """calculate C(n) mod m"""
    if n <= 2:
        return 1
    k = 2
    tk = 3
    pk = 2
    while k < n - 1:
        tk1 = (2 * pk * tk * tk) % m
        pk1 = (2 * pk * pk * tk) % m
        tk = tk1
        pk = pk1
        k += 1
    return (pk * pk * pk) % m

#print(cycles(5, pow(2, 60)))
#print(cycles(10000, pow(10, 8)))
#print(cycles(10000, pow(13, 8)))

def cycles_direct(n, m):
    """calculate C(n) mod m by direct formula"""
    if n <= 2:
        return 1
    two_exp = pow(3, n - 2)
    two = pow(2, two_exp, m)
    three_exp = 3 * (pow(3, n - 3) - 1) // 2
    three = pow(3, three_exp, m)
    return (two * three) % m

print(cycles_direct(5, pow(2, 60)))
print(cycles_direct(10000, pow(10, 8)))
print(cycles_direct(10000, pow(13, 8)))

def p312():
    n = 10000
    # step 1: plug in numbers
    mexp = 6749184
    m = 21934848
    two_exp = pow(3, n - 2, mexp)
    two = pow(2, two_exp, m)
    three_exp = 3 * (pow(3, n - 3, mexp) - 1) // 2
    three = pow(3, three_exp, m)
    n = (two * three) % m
    # step 2: ???
    mexp = 71288256
    m = 231686832
    two_exp = pow(3, n - 2, mexp)
    two = pow(2, two_exp, m)
    three_exp = 3 * (pow(3, n - 3, mexp) - 1) // 2
    three = pow(3, three_exp, m)
    n = (two * three) % m
    # step 3: profit!
    mexp = 12 * pow(13, 7)
    m = pow(13, 8)
    two_exp = pow(3, n - 2, mexp)
    two = pow(2, two_exp, m)
    three_exp = 3 * (pow(3, n - 3, mexp) - 1) // 2
    three = pow(3, three_exp, m)
    return (two * three) % m

print(p312())

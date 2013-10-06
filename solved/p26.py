#!/usr/bin/env python3

# long division
#
#     0.1428
#   +---------
# 7 | 1.000000
#     - 7
#     ---
#       30
#      -28
#       --
#        20
#       -14
#        --
#         60
#
#      0.0909
#    +---------
# 11 | 1.000000
#      - 0
#      ---
#      1 00
#       -99
#      ----
#         10
#         -0
#         --
#         100

def cycle_length(n):
    rems = []
    r = 1
    while r not in rems and r != 0:
        rems.append(r)
        r = (r * 10) % n
    if r == 0:
        return 0
    else:
        return len(rems) - rems.index(r)

def cycle_length2(n):
    r = 1
    for i in range(n):
        r = (r * 10) % n
    r0 = r
    l = 1
    r = (r * 10) % n
    while r != r0:
        r = (r * 10) % n
        l += 1
    return l

c_max = 0
k_max = 0
for k in range(2, 1000):
    c = cycle_length2(k)
    if c > c_max:
        c_max = c
        k_max = k

print(k_max, c_max)

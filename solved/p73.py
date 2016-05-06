from fractions import gcd
k = 0
for d in range(1, 12001):
    for n in range(d // 3 + 1, (d + 1) // 2):
        if gcd(n, d) == 1:
            k += 1
print(k)
from fractions import Fraction

target = Fraction(3, 7)
closest = Fraction(0, 1)
for d in range(1, 1000001):
    n = d * target.numerator // target.denominator
    f = Fraction(n, d)
    #print(d, n, f)
    if f > closest and f != target:
        closest = f
print(closest)
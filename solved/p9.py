for c in range(334, 501):
    for a in range(1, (1000 - c) // 2 + 1):
        b = 1000 - c - a
        if a ** 2 + b ** 2 == c ** 2:
            print(a,b,c,a*b*c)
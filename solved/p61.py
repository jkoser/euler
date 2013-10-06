#!/usr/bin/env python3

from euler3 import polygonals_below, cat_digits

ps = []
for r in range(3, 9):
    ps.append(set(polygonals_below(10000, r)))

def set_number(n):
    global ps
    for r in range(6):
        if n in ps[r]:
            return r
    return None

for h1 in range(10, 100):
    for h2 in range(10, 100):
        if set_number(cat_digits(h1, h2)) == None:
            continue
        for h3 in range(10, 100):
            if set_number(cat_digits(h2, h3)) == None:
                continue
            print(h1, h2, h3)
            for h4 in range(10, 100):
                if set_number(cat_digits(h3, h4)) == None:
                    continue
                for h5 in range(10, 100):
                    if set_number(cat_digits(h4, h5)) == None:
                        continue
                    for h6 in range(10, 100):
                        s = [cat_digits(h1, h2), cat_digits(h2, h3), \
                                cat_digits(h3, h4), cat_digits(h4, h5), \
                                cat_digits(h5, h6), cat_digits(h6, h1)]
                        seen = [False] * 6
                        match = False
                        for n in s:
                            match = False
                            for r in range(6):
                                if n in ps[r] and not seen[r]:
                                    seen[r] = True
                                    match = True
                                    break
                            if not match:
                                break
                        if match:
                            print(s, sum(s))

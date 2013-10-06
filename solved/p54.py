#!/usr/bin/env python3

import poker

c = 0
with open('p54.txt', 'r') as f:
    for line in f:
        cards = line.rstrip().split(' ')
        if poker.cmp_hands(cards[0:5], cards[5:10]) > 0:
            c += 1
print(c)

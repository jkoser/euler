import functools
import itertools

def is_flush(h):
    fs = map(lambda x: x[1], h)
    return functools.reduce(lambda x, y: x if x == y else False, fs)

def value(c):
    if c[0] == 'T':
        return 10
    elif c[0] == 'J':
        return 11
    elif c[0] == 'Q':
        return 12
    elif c[0] == 'K':
        return 13
    elif c[0] == 'A':
        return 14
    else:
        return int(c[0])

def is_straight(vs):
    if vs[0] + 1 == vs[1] and vs[0] + 2 == vs[2] and \
            vs[0] + 3 == vs[3] and vs[0] + 4 == vs[4]:
        return vs[4]
    else:
        return False

def is_of_a_kind(vs, n):
    v = False
    rest = []
    for k, g in itertools.groupby(vs):
        c = sum(1 for x in g)
        if c == n:
            v = k
        else:
            rest = rest + ([k] * c)
    rest.reverse()
    return [v] + rest if v else False

def is_two_pairs(vs):
    found = []
    other = 0
    for k, g in itertools.groupby(vs):
        if sum(1 for x in g) == 2:
            found.append(k)
        else:
            other = k
    if len(found) == 2:
        found.reverse()
        return found + [other]
    else:
        return False

def strength(h):
    flush = is_flush(h)
    values = list(map(value, h))
    values.sort()
    if values == [10, 11, 12, 13, 14] and flush:
        return [10]
    straight = is_straight(values)
    if straight and flush:
        return [9, straight]
    four = is_of_a_kind(values, 4)
    if four:
        return [8] + four
    three = is_of_a_kind(values, 3)
    two = is_of_a_kind(values, 2)
    if three and two:
        return [7, three[0], two[0]]
    if flush:
        values.reverse()
        return [6] + values
    if straight:
        return [5, straight]
    if three:
        return [4] + three
    two_pairs = is_two_pairs(values)
    if two_pairs:
        return [3] + two_pairs
    if two:
        return [2] + two
    else:
        values.reverse()
        return [1] + values

def cmp_hands(h1, h2):
    s1 = strength(h1)
    s2 = strength(h2)
    if s1 < s2:
        return -1
    elif s1 == s2:
        return 0
    else:
        return 1

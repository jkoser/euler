#!/usr/bin/env python3

# Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6,
# and each line adding to nine.

#    4
#      3
#    1   2   6
# 5

# Working clockwise, and starting from the group of three with the numerically
# lowest external node (4,3,2 in this example), each solution can be described
# uniquely. For example, the above solution can be described by the set:
# 4,3,2; 6,2,1; 5,1,3.

# It is possible to complete the ring with four different totals: 9, 10, 11,
# and 12. There are eight solutions in total.
# Total    Solution Set
# 9        4,2,3; 5,3,1; 6,1,2
# 9        4,3,2; 6,2,1; 5,1,3
# 10       2,3,5; 4,5,1; 6,1,3
# 10       2,5,3; 6,3,1; 4,1,5
# 11       1,4,6; 3,6,2; 5,2,4
# 11       1,6,4; 5,4,2; 3,2,6
# 12       1,5,6; 2,6,4; 3,4,5
# 12       1,6,5; 3,5,4; 2,4,6

# By concatenating each group it is possible to form 9-digit strings; the
# maximum string for a 3-gon ring is 432621513.

# Using the numbers 1 to 10, and depending on arrangements, it is possible to
# form 16- and 17-digit strings. What is the maximum 16-digit string for a
# "magic" 5-gon ring?

from itertools import permutations
import timeit

def threegon_solutions():
    """Generates solutions to the n=3 case.
    
    Yields:
        Pairs whose first element is the sum of values along each row, and
        whose second element is a tuple of nine value assignments using the
        clockwise notation.
    """
    unused = set(range(1, 7))
    # A solution starts with the lowest outside node, so it cannot be 5 or 6.
    for a in range(1, 5):
        u2 = unused.copy()
        u2.remove(a)
        for b, c in permutations(u2, 2):
            s = a + b + c
            if s in range(9, 13):
                u3 = u2 - set((b, c))
                for d, e, f in permutations(u3, 3):
                    if d < a or f < a:
                        continue
                    if d + c + e == s and f + e + b == s:
                        yield (s, (a, b, c, d, c, e, f, e, b))

def main():
    for s in threegon_solutions():
        print(s)

if __name__ == '__main__':
    t = timeit.timeit('main()', setup='from __main__ import main', number=1)
    print('running time:', t, 'seconds')
#!/usr/bin/env python3

# In the 5 by 5 matrix below, the minimal path sum from the top left to the
# bottom right, by moving left, right, up, and down, is indicated with
# asterisks and is equal to 2297.
#
# (131* 673  234* 103*  18*
#  201*  96* 342* 965  150*
#  630  803  746  422* 111*
#  537  699  497  121* 956
#  805  732  524   37* 331*)
#
# Find the minimal path sum, in matrix.txt, a 31K text file containing a 80
# by 80 matrix, from the top left to the bottom right by moving left, right,
# up, and down.


import heapq

# Load the matrix.
m = []
with open('p083_matrix.txt', 'r') as f:
    for line in f:
        line = line.rstrip()
        m.append(list(map(int, line.split(','))))
rows = len(m)
cols = len(m[0])

# Define some utility functions.
def is_valid_location(loc):
    i, j = loc
    return i >= 0 and i < rows and j >= 0 and j < cols
def neighbors(loc):
    i, j = loc
    return filter(is_valid_location, [(i-1,j), (i,j-1), (i,j+1), (i+1,j)])

# Discover the minimal path to each location according to Dijkstra's algorithm
# until the bottom-right is reached.
least_paths = {(0,0): m[0][0]}
to_visit = []
dest = (rows-1, cols-1)
for i, j in neighbors((0,0)):
    heapq.heappush(to_visit, (m[0][0] + m[i][j], i, j))
while True:
    p, i, j = heapq.heappop(to_visit)
    if (i, j) == dest:
        # Since we process shorter paths first, this must be best.
        print(p)
        break
    elif (i, j) in least_paths:
        # We've already found a shorter path to this location.
        continue
    else:
        least_paths[i, j] = p
        # Queue up neighbors that have not yet been visited.
        for i_, j_ in neighbors((i, j)):
            if (i_, j_) not in least_paths:
                heapq.heappush(to_visit, (p + m[i_][j_], i_, j_))

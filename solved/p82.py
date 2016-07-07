#!/usr/bin/env python3

m = []
with open('p082_matrix.txt', 'r') as f:
    for line in f:
        line = line.rstrip()
        m.append(list(map(int, line.split(','))))
n = len(m)  # assume matrix is square, because it is

# compute cost of minimum paths by dynamic programming
least_path = [[m[i][0]] for i in range(n)]
for j in range(1, n):
    # down and right
    least_path[0].append(least_path[0][j-1] + m[0][j])
    for i in range(1, n):
        least_path[i].append(min(least_path[i-1][j], least_path[i][j-1]) + m[i][j])
    # up
    for i in range(n - 2, -1, -1):
        least_path[i][j] = min(least_path[i][j], least_path[i+1][j] + m[i][j])

# output least path over all possible endpoints
r = float('inf')
for i in range(n):
    r = min(least_path[i][n-1], r)
print(r)
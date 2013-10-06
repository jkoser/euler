#!/usr/bin/env python3

m = []
with open('p81.txt', 'r') as f:
    for line in f:
        line = line.rstrip()
        m.append(list(map(int, line.split(','))))

least_path = []
for i in range(len(m)):
    least_path.append([])
    for j in range(len(m[i])):
        if i == 0:
            if j == 0:
                least_path[0].append(m[0][0])
            else:
                least_path[0].append(least_path[0][j - 1] + m[0][j])
        elif j == 0:
            least_path[i].append(least_path[i - 1][0] + m[i][0])
        else:
            least_path[i].append(min(least_path[i - 1][j], least_path[i][j - 1]) + m[i][j])

print(least_path[len(m) - 1][len(m[0]) - 1])

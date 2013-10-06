import string

names = raw_input()
names = names.replace('"', '').split(',')
names.sort()
sum = 0
for i, name in enumerate(names):
	ssum = 0
	for c in name:
		ssum += string.ascii_uppercase.index(c) + 1
	sum += (i + 1) * ssum
print sum

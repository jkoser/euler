from euler3 import totients_below
print(sum(map(lambda x: x[1], totients_below(1000001))))
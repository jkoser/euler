#!/usr/bin/python3

import time

def distrib_products_from(n, k, i):
    if n < i * k:
        return
    elif k == 1:
        yield n
        return
    else:
        for m in range(i, n // k + 1):
            for rest in distrib_products_from(n - m, k - 1, m):
                yield m * rest

def distrib_products(n, k):
    return distrib_products_from(n, k, 1)

def is_product_sum(n, k):
    return n in distrib_products(n, k)

def min_product_sum(k):
    n = k
    while True:
        if is_product_sum(n, k):
            return n
        n += 1

start_time = time.time()
for k in range(2, 100):
    print(k, min_product_sum(k))
end_time = time.time()
print('time: ', end_time - start_time, 's', sep='')

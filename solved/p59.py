#!/usr/bin/env python3

# p59.txt uses XOR encryption with a key of 3 lowercase letters.
# Find the sum of the ASCII values in the original text.

import string

def xor_with_key(text, key):
    xor = []
    keylen = len(key)
    for i in range(len(text)):
        xor.append(text[i] ^ key[i % keylen])
    return xor

def ords_to_string(ords):
    return ''.join(map(chr, ords))

ciphertext = []
with open('p59.txt', 'r') as f:
    for line in f:
        line = line.rstrip()
        ciphertext = list(map(int, line.split(',')))
#print(ciphertext)
#print(repr(''.join(map(chr, ciphertext))))

for a in string.ascii_lowercase:
    for b in string.ascii_lowercase:
        for c in string.ascii_lowercase:
            attempt = xor_with_key(ciphertext, [ord(a), ord(b), ord(c)])
            attempt_str = ords_to_string(attempt)
            if 'the' in attempt_str and 'and' in attempt_str and 'in' in attempt_str:
                print(repr(attempt_str))
                print(a + b + c)
                print(sum(attempt))

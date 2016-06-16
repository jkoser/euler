# A common security method used for online banking is to ask the user for three
# random characters from a passcode. For example, if the passcode was 531278,
# they may ask for the 2nd, 3rd, and 5th characters; the expected reply would
# be: 317.
#
# The text file, keylog.txt, contains fifty successful login attempts.
#
# Given that the three characters are always asked for in order, analyse the
# file so as to determine the shortest possible secret passcode of unknown
# length.


from collections import deque
import sys


# Use a set to ignore duplicates.
triples = set()
with open('p079_keylog.txt', 'r') as f:
    for line in f:
        triples.add(line[0:3])

# BFS state consists of a passcode prefix and a collection of (non-empty)
# character sequences that must appear in the remainder of the passcode.
initial_bfs_state = ('', triples)
bfs_states = deque([initial_bfs_state])

# Our passcode requirements are finite, and all transitions make progress,
# so we will find a solution and break out of the loop.
while True:
    prefix, seqs = bfs_states.popleft()
    for c in {s[0] for s in seqs}:
        next_prefix = prefix + c
        next_seqs = set()
        for s in seqs:
            if s[0] != c:
                next_seqs.add(s)
            elif s != c:
                next_seqs.add(s[1:])
        if next_seqs:
            bfs_states.append((next_prefix, next_seqs))
        else:
            print(next_prefix)
            sys.exit()

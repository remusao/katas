

import sys
for line in sys.stdin:
    for c in line:
        if not (0 <= ord(c) <= 127):
            print(line)

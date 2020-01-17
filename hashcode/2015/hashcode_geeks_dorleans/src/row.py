#! /usr/bin/env python
# -*- coding: utf-8 -*-


class Row(object):
    def __init__(self, row_index, size, unavailables):
        self.index = row_index
        if len(unavailables) == 0:
            self.blocks = [(0, size)]
        else:
            unavailables = sorted(unavailables)
            self.size = size
            self.blocks = []

            last = 0
            for u in unavailables:
                if (u - last) > 0:
                    self.blocks.append((last, u - last))
                last = u + 1
            if last < size:
                self.blocks.append((last, size - unavailables[-1] - 1))
            self.blocks.sort(reverse=True, key=lambda b: b[1])

    def fit(self, size):
        for b in self.blocks:
            if b[1] == size:
                return True
        return False

    def __contains__(self, size):
        return len(filter(lambda b: size <= b, [b[1] for b in self.blocks]))

    def freeslots(self):
        return sum([b[1] for b in self.blocks])

    def getmax(self):
        return self.blocks[0][1]

    def getmin(self):
        return self.blocks[-1][1]

    def useslot(self, size, blocksize=-1):
        pos, blocksize = self.blocks[0]
        assert blocksize >= size
        for s in self.blocks:
            if s[1] < size:
                break
            else:
                pos, blocksize = s

        index = self.blocks.index((pos, blocksize))
        self.blocks.pop(index)
        if size < blocksize:
            self.blocks.append((pos + size, blocksize - size))
            self.blocks.sort(reverse=True, key=lambda b: b[1])
        elif size > blocksize:
            assert False
        return pos

import sys



def main(f):
    t = int(f.readline())
    while t != -1:
        candies = [int(f.readline()) for x in xrange(0, t)]
        s = sum(candies)
        # Solve
        if s % t != 0:
            print -1
        else:
            res, i, mean = 0, 0, s / t
            candies.sort()
            while candies[i] < mean:
                res += mean - candies[i]
                i += 1
            print res
        t = int(f.readline())



if __name__ == '__main__':
    f = sys.stdin
    main(f)

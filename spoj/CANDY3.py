
import sys

def main(f):
    problemInput = f.read().split('\n')
    _t = int(problemInput[0])
    problemInput = problemInput[2:]
    for t in xrange(0, _t):
        N = int(problemInput[0])
        if sum(map(int, problemInput[1:N+1])) % N == 0:
            print 'YES'
        else:
            print 'NO'
        problemInput = problemInput[N+2:]




if __name__ == '__main__':
    f = sys.stdin
    main(f)

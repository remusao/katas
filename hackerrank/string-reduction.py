
from collections import Counter
def stringReduction(a):
    count = Counter(a)
    if len(count) == 1:
        return len(a)
    elif count['a'] % 2 == 0 and count['b'] % 2 == 0 and count['c'] % 2 == 0:
        return 2
    elif count['a'] % 2 != 0 and count['b'] % 2 != 0 and count['c'] % 2 != 0:
        return 2
    else:
        return 1

if __name__ == '__main__':
    t = int(input())
    for i in range(0,t):
        a=input()
        print(stringReduction(a))

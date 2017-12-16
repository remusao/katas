import operator

def lonelyinteger(a):
    return reduce(operator.xor, a)

if __name__ == '__main__':
    a = input()
    b = map(int, raw_input().strip().split(" "))
    print lonelyinteger(b)

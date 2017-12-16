
from decimal import *
getcontext().prec = 200

def main():
    total = 0
    for n in range(2, 101):
        s = Decimal(n).sqrt()
        if round(s) != s:
            for i in range(100):
                total += int(s % 10)
                s *= 10
    return total

if __name__ == '__main__':
    print(main())

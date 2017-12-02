
from __future__ import print_function
import sys

NUM = {
    'M': 1000,
    'D': 500,
    'C': 100,
    'L': 50,
    'X': 10,
    'V': 5,
    'I': 1
}


CONVERT = [
    (1000, 'M'),
    (900, 'CM'),
    (500, 'D'),
    (400, 'CD'),
    (100, 'C'),
    (90, 'XC'),
    (50, 'L'),
    (40, 'XL'),
    (10, 'X'),
    (9, 'IX'),
    (5, 'V'),
    (4, 'IV'),
    (1, 'I')
]


def tobase10(num):
    num = num.replace('CM', 'DCCCC')
    num = num.replace('CD', 'CCCC')
    num = num.replace('XC', 'LXXXX')
    num = num.replace('XL', 'XXXX')
    num = num.replace('IIXX', 'XVIII')
    num = num.replace('IX', 'VIIII')
    num = num.replace('IIX', 'VIII')
    num = num.replace('IV', 'IIII')
    return sum(NUM[c] for c in num)


def minimize(num):
    result = ''
    number = tobase10(num)
    while number > 0:
        for val, symbol in CONVERT:
            if val <= number:
                if 'D' in symbol and 'D' in result:
                    continue
                if 'L' in symbol and 'L' in result:
                    continue
                if 'V' in symbol and 'V' in result:
                    continue
                result += symbol
                number -= val
                break
    return result


def main():
    res = 0
    with open(sys.argv[1], 'r') as num_input:
        numbers = num_input.read().split()
        for number in numbers:
            minimized = minimize(number)
            res += len(number) - len(minimized)
    print(res)


if __name__ == "__main__":
    main()

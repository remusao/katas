
def rev(n):
    r = 0
    while n > 0:
        r = r * 10 + (n % 10)
        n = n // 10
    return r


def even(n):
    return n % 2 == 0


def odd(n):
    return not even(n)


def solve(limit):
    memo = {}
    def reversible(i):
        if i == 0:
            return True
        elif i in memo:
            return memo[i]
        else:
            new_i = i // 10
            if new_i in memo:
                rec_res = memo[new_i]
            else:
                rec_res = reversible(new_i)
            result = odd(i % 10) and rec_res
            memo[i] = result
            return result

    count = 0
    for n in range(1, limit):
        if n % 1000000 == 0:
            print n
        rev_n = rev(n)
        if n % 10 == 0 or rev_n % 10 == 0:
            continue
        elif reversible(n + rev_n):
            count += 1
    return count


def main():
    print solve(1000000000)


main()

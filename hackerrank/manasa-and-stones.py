def display(values):
    print(" ".join(map(str, sorted(values))))

def main():
    t = int(input())
    for _ in range(t):
        n = int(input())
        a = int(input())
        b = int(input())
        values = set([a, b])
        if n == 1: print("0")
        elif n == 2: display(values)
        else:
            for _ in range(n - 2):
                next_level = set()
                for v in values:
                    next_level.add(v + a)
                    next_level.add(v + b)
                values = next_level
            display(values)


if __name__ == "__main__":
    main()

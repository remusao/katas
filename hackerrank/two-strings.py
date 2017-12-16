import fileinput

def main():
    stdin = fileinput.input()
    n = int(next(stdin))
    for _ in range(n):
        s1 = next(stdin).strip()
        s2 = next(stdin).strip()
        if set(s1).intersection(set(s2)):
            print("YES")
        else:
            print("NO")


if __name__ == "__main__":
    main()

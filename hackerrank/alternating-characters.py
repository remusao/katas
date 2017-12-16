n = int(raw_input())
for _ in range(n):
    s = raw_input()
    deleted = 0
    last = s[0]
    for letter in s[1:]:
        if letter == last:
            deleted += 1
        else:
            last = letter
    print deleted

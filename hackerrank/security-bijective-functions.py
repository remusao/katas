
n = int(input())
fx = list(map(int, input().split()))

if len(set(fx)) == len(fx): print("YES")
else: print("NO")

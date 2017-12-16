def max_subarray(A):
    max_ending_here = A[0]
    max_so_far = max_ending_here
    for x in A[1:]:
        max_ending_here = max(x, max_ending_here + x)
        max_so_far = max(max_so_far, max_ending_here)
    return max_so_far

t = int(input())
for _ in range(t):
    input()
    arr = list(map(int, input().split()))
    positives = list(filter(lambda n: n > 0, arr))
    if len(positives) == 0:
        not_contiguous = max(arr)
    else:
        not_contiguous = sum(positives)
    print(max_subarray(arr), not_contiguous)

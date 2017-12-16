#include <stdio.h>

int main()
{
    int n, a, b, count = 0;
    scanf("%d", &n);
    for (a = 1; a <= n; ++a)
        for (b = a; b <= (n / a); ++b)
            ++count;
    printf("%d\n", count);
    return 0;
}

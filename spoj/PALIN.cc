#include <cstdio>
#include <cstring>

#define SIZE 1000000
#define SHIFT 1000


unsigned incr(char number[], unsigned offset)
{
    bool ret = false;

    do
    {
        ret = false;
        if (number[offset] == '9')
        {
            ret = true;
            number[offset--] = '0';
        }
        else
            ++number[offset];
    } while (ret);

    return offset;
}

unsigned nextPalindrome(char notShifted[], char number[], unsigned shift)
{
    unsigned len = strlen(number) - 1;
    unsigned i = 0;

    // Add one to the number because we want a palindrom
    // bigger than initial number
    if (shift == SHIFT && incr(notShifted, len + shift) < shift ? 1 : 0)
        return 1;

    while (i < len)
    {
        if (number[len - i] > number[i] && incr(notShifted, len - i - 1 + shift) < shift ? 1 : 0)
            return 1;
        number[len - i] = number[i];
        ++i;
    }

    return 0;
}

int main()
{
    char number[SIZE + SHIFT + 1];
    unsigned t, overflow, shift;

    scanf("%u", &t);
    while (t--)
    {
        shift = SHIFT;
        overflow = 0;
        memset(number, '0', SIZE + SHIFT + 1);
        scanf("%s", number + shift);

        // Eliminate leading zeros
        while (number[shift] == '0')
            ++shift;

        // Solution
        while ((overflow = nextPalindrome(number, number + shift, shift)))
            --shift;

        // print solution
        printf("%s\n", number + shift);
    }
}

#include <iostream>
using namespace std;

/*
 * Complete the function below.
 */
int maxXor(int l, int r)
{
    unsigned maxi = 0;
    for (unsigned a = l; a <= r; ++a)
    {
        for (unsigned b = a; b <= r; ++b)
        {
            if ((a ^ b) > maxi)
                maxi = a ^ b;
        }
    }
    return maxi;
}

int main() {
    int res;
    int _l;
    cin >> _l;

    int _r;
    cin >> _r;

    res = maxXor(_l, _r);
    cout << res;

    return 0;
}

#include <iostream>
#include <math.h>

inline bool is_kaprekar(long long int n, int r) {
    long long int p = n * n;
    // std::cout << p << ' ' << r << ' ' << (p / r) << ' ' << (p % r) << std::endl;
    return (p / r + p % r) == n;
}

int main() {
    int p, q;
    std::cin >> p >> q;

    int count = 0;
    while (p <= q) {
        if (is_kaprekar(p, pow(10, (int)log10(p) + 1))) { std::cout << p << ' '; ++count; }
        ++p;
    }
    if (count == 0) std::cout << "INVALID RANGE" << std::endl;
    return 0;
}

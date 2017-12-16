#include <cstdio>
#include <iostream>


int main() {
    int n, number;
    float neg = 0;
    float zero = 0;
    float pos = 0;

    std::cin >> n;
    for (int i = 0; i < n; ++i) {
        std::cin >> number;
        if (number < 0) neg += 1;
        else if (number == 0) zero += 1;
        else pos += 1;
    }
    printf("%.4f\n%.4f\n%.4f\n", pos / n, neg / n, zero / n);
    return 0;
}

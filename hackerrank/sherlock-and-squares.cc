#include <iostream>
#include <cmath>


int main() {
    int t, a, b;
    std::cin >> t;

    while (t--) {
        std::cin >> a >> b;
        int res = 0;
        int square;
        for (int i = 1; i < 32000; ++i) {
            square = i * i;
            if (square >= a && square <= b) ++res;
        }
        std::cout << res << std::endl;
    }
    return 0;
}

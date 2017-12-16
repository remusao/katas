#include <iostream>

int main() {
    int t, n, k, present, arrival;
    std::cin >> t;

    while (t--) {
        std::cin >> n;
        std::cin >> k;
        present = 0;

        while (n--) {
            std::cin >> arrival;
            if (arrival <= 0) ++present;
        }

        if (present >= k) std::cout << "NO" << std::endl;
        else std::cout << "YES" << std::endl;
    }

    return 0;
}

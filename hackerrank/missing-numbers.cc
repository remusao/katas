#include <iostream>
#include <array>


int main() {
    int n, m, value;
    std::array<int, 10002> c1, c2;
    c1.fill(0);
    c2.fill(0);

    // Read numbers
    std::cin >> n;
    while (n--) {
        std::cin >> value;
        ++c1[value];
    }

    std::cin >> m;
    while (m--) {
        std::cin >> value;
        ++c2[value];

    }

    for (int i = 1; i <= 10001; ++i) {
        if (c1[i] != c2[i]) {
            std::cout << i << ' ';
        }
    }
    std::cout << std::endl;

    return 0;
}

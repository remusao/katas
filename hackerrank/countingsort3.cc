#include <array>
#include <string>
#include <iostream>


int main() {
    // Init counter
    std::array<int, 100> counter;
    for (int i = 0; i < 100; ++i) counter[i] = 0;

    int n, elt;
    std::string value;
    std::cin >> n;

    while (n--) {
        std::cin >> elt;
        std::cin >> value;
        ++counter[elt];
    }

    int cumulative_sum = 0;
    for (int i = 0; i < 100; ++i) {
        for (int j = 0; j < counter[i]; ++j) {
            ++cumulative_sum;
        }
        std::cout << cumulative_sum << ' ';
    }
    std::cout << std::endl;

    return 0;
}

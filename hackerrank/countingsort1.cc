#include <array>
#include <iostream>


int main() {
    // Init counter
    std::array<int, 100> counter;
    for (int i = 0; i < 100; ++i) counter[i] = 0;

    int n, elt;
    std::cin >> n;

    while (n--) {
        std::cin >> elt;
        ++counter[elt];
    }

    for (int count: counter) std::cout << count << ' ';
    std::cout << std::endl;

    return 0;
}

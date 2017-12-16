#include <list>
#include <array>
#include <string>
#include <iostream>


int main() {
    // Init counter
    std::array<std::list<std::string>, 100> counter;

    int n, elt, mid;
    std::string value;
    std::cin >> n;
    mid = n / 2;

    while (n--) {
        std::cin >> elt >> value;
        if (n >= mid) value = "-";
        counter[elt].emplace_back(std::move(value));
    }

    for (const auto& lst: counter) {
        for (const auto& str: lst) {
            std::cout << str << ' ';
        }
    }
    std::cout << std::endl;

    return 0;
}

#include <iostream>
#include <string>
#include <array>


int main() {
    std::string s;
    std::cin >> s;

    std::array<int, 26> counter;
    counter.fill(0);

    for (char c: s) {
        ++counter[c - 'a'];
    }

    int odd = 0;
    for (int count: counter) {
        if (count > 0 && count % 2 != 0) ++odd;
    }

    if (odd <= 1) std::cout << "YES" << std::endl;
    else std::cout << "NO" << std::endl;

    return 0;
}

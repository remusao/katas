#include <iostream>
#include <inttypes.h>


int main() {
    int n, number;
    uint64_t sum = 0;
    std::cin >> n;
    while (n--) {
        std::cin >> number;
        sum += number;
    }
    std::cout << sum << std::endl;
    return 0;
}

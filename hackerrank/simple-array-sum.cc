#include <iostream>

int main() {
    int n, num;
    int sum = 0;
    std::cin >> n;
    while (n--) {
        std::cin >> num;
        sum += num;
    }
    std::cout << sum << std::endl;
    return 0;
}

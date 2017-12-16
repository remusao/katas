#include <iostream>


int main() {
    int n;
    std::cin >> n;

    for (int i = 0; i < n; ++i) {
        int j = 0;
        for (; j < (n - i - 1); ++j) {
            std::cout << " ";
        }
        for (; j < n; ++j) {
            std::cout << "#";
        }
        std::cout << std::endl;
    }
    return 0;
}

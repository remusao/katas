#include <iostream>


int main() {
    int n, num;
    long long int result = 0;
    std::cin >> n;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            // Read next number
            std::cin >> num;
            // If on first diag
            if (i == j) result += num;
            // If on second diag
            if ((i + j) == (n - 1)) result -= num;
        }
    }
    std::cout << (result > 0 ? result : -result) << std::endl;
    return 0;
}

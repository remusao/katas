#include <iostream>
#include <string>
#include <array>
#include <vector>


int DP[5001][5001];

void display_DP(const std::string& a, int last_r, const std::string& b, int last_c) {
    // Display DP
    std::cout << "  ";
    for (char c: b) std::cout << c << ' ';
    std::cout << std::endl;
    for (int i = 0; i <= last_r; ++i) {
        std::cout << a[i] << ' ';
        for (int j = 0; j <= last_c; ++j) {
            std::cout << DP[i][j] << ' ';
        }
        std::cout << std::endl;
    }
}


int solve(const std::string& a, const std::string& b) {
    for (size_t i = 0; i < b.size(); ++i) DP[i][0] = 0;
    for (size_t i = 0; i < a.size(); ++i) DP[0][i] = 0;

    for (size_t i = 1; i <= b.size(); ++i) {
        for (size_t j = 1; j <= a.size(); ++j) {
            if (b[i - 1] == a[j - 1]) {
                DP[i][j] = 1 + DP[i - 1][j - 1];
            }
            else {
                DP[i][j] = std::max(DP[i - 1][j], DP[i][j - 1]);
            }
        }
    }

    return DP[b.size()][a.size()];
}


int main() {
    std::string a, b;
    std::cin >> a >> b;

    if (a.size() == 0 || b.size() == 0) std::cout << 0 << std::endl;
    else std::cout << solve(a, b) << std::endl;
    return 0;
}

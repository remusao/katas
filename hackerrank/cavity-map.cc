#include <iostream>
#include <vector>
#include <string>


int main() {
    int n;
    std::cin >> n;
    std::vector<std::string> grid{n};

    for(int i = 0; i < n; ++i) {
        std::cin >> grid[i];
    }

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (j == 0 || i == 0 || j == (n - 1) || i == (n - 1)) {
                std::cout << grid[i][j];
            }
            else {
                char h = grid[i][j];
                if (h > grid[i - 1][j] && h > grid[i + 1][j] && h > grid[i][j - 1] && h > grid[i][j + 1]) {
                    std::cout << 'X';
                }
                else std::cout << h;
            }
        }
        std::cout << std::endl;
    }
    return 0;
}

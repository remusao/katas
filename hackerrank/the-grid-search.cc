#include <iostream>
#include <vector>
#include <string>


std::vector<std::string> read_pattern(size_t r) {
    std::string line;
    std::vector<std::string> result;
    result.reserve(r);

    for (int i = 0; i < r; ++i) {
        std::cin >> line;
        result.emplace_back(std::move(line));
    }

    return std::move(result);
}

template <typename M>
bool compare(const M& grid, const M& pattern, size_t p_rows, size_t p_cols, size_t i, size_t j) {
    size_t k = 0;
    size_t l = 0;
    for (; k < p_rows; ++k) {
        for (l = 0; l < p_cols; ++l) {
            if (grid[i + k][j + l] != pattern[k][l]) return false;
        }
    }

    return true;
}

bool find_pattern() {
    size_t g_rows, g_cols;
    size_t p_rows, p_cols;

    std::cin >> g_rows >> g_cols;
    auto grid = read_pattern(g_rows);

    std::cin >> p_rows >> p_cols;
    auto pattern = read_pattern(p_rows);
    char first_elt = pattern[0][0];

    for (size_t i = 0; i <= (g_rows - p_rows); ++i) {
         for (size_t j = 0; j <= (g_cols - p_cols); ++j) {
             if (grid[i][j] == first_elt && compare(grid, pattern, p_rows, p_cols, i, j)) {
                 return true;
             }
        }
    }
    return false;
}

int main() {
    int t;
    std::cin >> t;

    while (t--) {
        if (find_pattern()) std::cout << "YES" << std::endl;
        else std::cout << "NO" << std::endl;
    }
    return 0;
}

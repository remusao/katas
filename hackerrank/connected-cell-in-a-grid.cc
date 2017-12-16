#include <vector>
#include <iostream>
#include <queue>


int main() {
    size_t m, n;
    int value;
    std::cin >> m >> n;
    std::vector<std::vector<bool>> grid;
    grid.resize(m);
    for (int i = 0; i < m; ++i) {
        grid[i].resize(n);
        for (int j = 0; j < n; ++j) {
            std::cin >> value;
            grid[i][j] = value == 1 ? true : false;
        }
    }

    std::vector<std::vector<bool>> mask;
    mask.resize(m);
    for (int i = 0; i < m; ++i) {
        mask[i].resize(n);
        for (int j = 0; j < n; ++j) {
            mask[i][j] = !grid[i][j];
        }
    }

    int max_size = 0;
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            if (!mask[i][j]) {
                int size = 0;
                mask[i][j] = true;
                std::queue<std::pair<int, int>> q;
                q.emplace(i, j);

                while (!q.empty()) {
                    ++size;
                    std::pair<int, int> pos = q.front();
                    q.pop();
                    for (int k = -1; k < 2; ++k) {
                        for (int l = -1; l < 2; ++l) {
                            std::pair<int, int> next = {pos.first + k, pos.second + l};
                            if (next.first >= 0 && next.first < m && next.second >= 0 && next.second < n && !mask[next.first][next.second]) {
                                mask[next.first][next.second] = true;
                                q.push(next);
                            }
                        }
                    }
                }
                if (size > max_size) max_size = size;
            }
        }
    }
    std::cout << max_size << std::endl;
    return 0;
}

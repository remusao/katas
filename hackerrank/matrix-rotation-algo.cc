#include <iostream>
#include <stdio.h>
#include <vector>

void rotate(int** matrix, int r, int c, int start_r, int start_c, int R, int** result) {
    std::vector<int> layer;
    int i = 0;
    int j = 0;
    do {
        layer.push_back(matrix[i + start_r][j + start_c]);
        // Compute next position
        if (i == 0 && j > 0) --j;
        else if (j == 0 && i < (r - 1)) ++i;
        else if (i == (r - 1) && j < (c - 1)) ++j;
        else --i;
    } while (i != 0 || j != 0);

    int size = layer.size();
    i = 0;
    j = 0;
    int index = 0;
    do {
        result[i + start_r][j + start_c] = layer[(index + (size - R % size)) % size];
        ++index;
        // Compute next position
        if (i == 0 && j > 0) --j;
        else if (j == 0 && i < (r - 1)) ++i;
        else if (i == (r - 1) && j < (c - 1)) ++j;
        else --i;
    } while (i != 0 || j != 0);
}

int main() {
    int m, n, r;
    std::cin >> m >> n >> r;

    // Read matrix
    int** matrix = new int*[m];
    int** rotated = new int*[m];
    for (int i = 0; i < m; ++i) {
        matrix[i] = new int[n];
        rotated[i] = new int[n];
        for (int j = 0; j < n; ++j) {
            std::cin >> matrix[i][j];
        }
    }

    // Do rotation
    for (int i = 0; i < (std::min(m, n) / 2); ++i) {
        rotate(matrix, m - i * 2, n - i * 2, i, i, r, rotated);
    }

    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            std::cout << rotated[i][j] << ' ';
        }
        std::cout << std::endl;
    }
    return 0;
}

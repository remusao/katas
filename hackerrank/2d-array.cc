#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;


int hourglass(int matrix[6][6], int r, int c) {
    if (r <= 3 && c <= 3) {
        return (matrix[r][c]
                + matrix[r][c + 1]
                + matrix[r][c + 2]
                + matrix[r + 1][c + 1]
                + matrix[r + 2][c]
                + matrix[r + 2][c + 1]
                + matrix[r + 2][c + 2]);
    }

    return 0;
}

int main() {
    int matrix[6][6];

    for (int i = 0; i < 6; ++i) {
        for (int j = 0; j < 6; ++j) {
            std::cin >> matrix[i][j];
        }
    }

    int largest = -(7 * 9);
    for (int i = 0; i <= 3; ++i) {
        for (int j = 0; j <= 3; ++j) {
            int sum = hourglass(matrix, i, j);
            if (sum > largest) largest = sum;
        }
    }

    std::cout << largest << std::endl;

    return 0;
}

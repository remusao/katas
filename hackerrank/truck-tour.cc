#include <iostream>
#include <vector>

int main() {
    int N;
    std::cin >> N;

    std::vector<int> data;
    data.reserve(N);

    int petrol, distance;
    for (int i = 0; i < N; ++i) {
        std::cin >> petrol >> distance;
        data.push_back(petrol - distance);
    }

    int i, j, value;
    for (i = 0; i < N; ++i) {
        value = data[i];
        j = 1;
        for (; j < N && value >= 0; ++j) {
            value += data[(i + j) % N];
        }
        if (j == N) {
            std::cout << i << std::endl;
            break;
        }
    }

    return 0;
}

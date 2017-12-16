#include <iostream>
#include <string>
#include <array>
#include <vector>


int main() {
    int n, m;
    std::cin >> n >> m;
    std::vector<std::string> topic{n};
    std::array<int, 501> counter;
    counter.fill(0);

    for (int i = 0; i < n; ++i) {
       std::cin >> topic[i];
    }

    int max = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (i != j) {
                // Count topics
                int count = 0;
                for (int k = 0; k < m; ++k) {
                    if (topic[i][k] == '1' || topic[j][k] == '1') ++count;
                }
                ++counter[count];
                if (count > max) max = count;
            }
        }
    }

    std::cout << max << std::endl;
    std::cout << (counter[max] / 2) << std::endl;
    return 0;
}

#include <vector>
#include <iostream>


int main() {
    int n, t, width;
    std::cin >> n >> t;

    std::vector<int> lane;
    lane.reserve(n);
    while (n--) {
        std::cin >> width;
        lane.push_back(width);
    }

    int i, j;
    while (t--) {
        std::cin >> i >> j;
        int mini = 3;
        for (; i <= j; ++i) {
            if (lane[i] < mini) mini = lane[i];
        }

        if (mini >= 3) std::cout << 3 << std::endl;
        else if (mini >= 2) std::cout << 2 << std::endl;
        else if (mini >= 1) std::cout << 1 << std::endl;
        else std::cout << 0 << std::endl;
    }
    return 0;
}

#include <vector>
#include <iostream>


bool solve(const std::vector<int>& vec) {
    int left = 0;
    int right = 0;
    for (int i: vec) right += i;

    for (int i: vec) {
        right -= i;
        if (right == left) {
            return true;
        }
        left += i;
    }

    return false;
}


int main() {
    int n, size, value;
    std::cin >> n;

    while (n--) {
        std::cin >> size;
        std::vector<int> vec;
        vec.reserve(size);

        for (int i = 0; i < size; ++i) {
            std::cin >> value;
            vec.push_back(value);
        }

        if (solve(vec)) {
            std::cout << "YES" << std::endl;
        }
        else {
            std::cout << "NO" << std::endl;
        }
    }
    return 0;
}

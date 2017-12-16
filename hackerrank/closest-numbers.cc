#include <list>
#include <iostream>
#include <algorithm>
#include <climits>
using namespace std;


int main() {
    int n, elt;
    std::vector<int> vec;

    // Read input
    std::cin >> n;
    vec.reserve(n);

    for (int i = 0; i < n; ++i) {
        std::cin >> elt;
        vec.push_back(elt);
    }

    // Sort numbers
    std::sort(vec.begin(), vec.end());

    // Find result
    int min_diff = INT_MAX;
    std::list<std::pair<int, int>> pairs;
    for (int i = 1; i < n; ++i) {
        int diff = vec[i] - vec[i - 1];
        if (diff == min_diff) pairs.emplace_back(vec[i - 1], vec[i]);
        else if (diff < min_diff) {
            pairs.clear();
            pairs.emplace_back(vec[i - 1], vec[i]);
            min_diff = diff;
        }
    }

    // Display results
    for (const auto& pair: pairs) {
        std::cout << pair.first << ' ' << pair.second << ' ';
    }
    std::cout << std::endl;

    return 0;
}

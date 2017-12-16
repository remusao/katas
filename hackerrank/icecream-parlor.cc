#include <algorithm>
#include <iostream>


int main() {
    int t, m, n;
    std::cin >> t;
    while (t--) {
        std::cin >> m >> n;
        std::vector<int> prices;
        prices.resize(n);
        for (int i = 0; i < n; ++i) std::cin >> prices[i];

        // Sort prices
        std::vector<int> indices;
        indices.resize(n);
        for (int i = 0; i < n; ++i) indices[i] = i;
        std::sort(indices.begin(), indices.end(), [&prices](int i, int j) {
            return prices[i] < prices[j];
        });

        int i = 0, j = n - 1;
        int sum = prices[indices[i]] + prices[indices[j]];
        while (sum != m) {
            if (sum > m) --j;
            else ++i;
            sum = prices[indices[i]] + prices[indices[j]];
        }
        if (indices[i] < indices[j])
            std::cout << (indices[i] + 1) << ' ' << (indices[j] + 1) << std::endl;
        else
            std::cout << (indices[j] + 1) << ' ' << (indices[i] + 1) << std::endl;
    }
    return 0;
}

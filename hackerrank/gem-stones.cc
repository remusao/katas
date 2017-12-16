#include <vector>
#include <iostream>
#include <string>

int main() {
    int n;
    std::string buffer;
    std::vector<int> counter(26, 0);

    std::cin >> n;
    for (int i = 0; i < n; ++i) {
        std::cin >> buffer;
        std::vector<bool> mask(26, false);
        for (char c: buffer) {
            mask[c - 'a'] = true;
        }

        for (int j = 0; j < 26; ++j) {
            if (mask[j]) ++counter[j];
        }
    }

    int result = 0;
    for (int i = 0; i < n; ++i) {
        if (counter[i] == n) ++result;
    }

    std::cout << result << std::endl;

    return 0;
}

#include <iostream>
#include <string>
#include <array>
#include <vector>


inline bool is_anagram(const std::array<int, 26>& c1, const std::array<int, 26>& c2) {
    for (int i = 0; i < 26; ++i) if (c1[i] != c2[i]) return false;
    return true;
}


int main() {
    int n;
    std::string s;

    std::cin >> n;
    while (n--) {
        std::cin >> s;
        int size = s.size();
        std::array<int, 26> counters[size][size];

        // Pre-compute counters
        for (int i = 0; i < size; ++i) {
            for (int j = i; j < size; ++j) {
                counters[i][j].fill(0);
                for (int k = i; k <= j; ++k) counters[i][j][s[k] - 'a']++;
            }
        }

        int count = 0;
        for (int i = 0; i < size; ++i) {
            for (int j = i; j < size; ++j) {
                // Our first substring is s[i, j]
                // Then we iterate over all other substrings
                for (int k = i; k < size; ++k) {
                    for (int l = k; l < size; ++l) {
                        if (k == i && l == j) continue;
                        if (is_anagram(counters[i][j], counters[k][l])) {
                            // std::cout << s.substr(k, l - k + 1) << ' ' << s.substr(i, j - i + 1) << std::endl;
                            ++count;
                        }
                    }
                }
            }
        }
        std::cout << count << std::endl;
    }
    return 0;
}

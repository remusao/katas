#include <vector>
#include <string>
#include <array>
#include <iostream>


inline bool is_valid(const std::array<int, 26>& count) {
    int index = 0;
    while (count[index] == 0) ++index;

    int value = count[index++];
    for (; index < 26; ++index) {
        if (count[index] > 0 && count[index] != value) return false;
    }
    return true;
}


bool solve(const std::string& str) {
    // Init counter
    std::array<int, 26> counter;
    counter.fill(0);

    // Count char
    for (char c: str) ++counter[c - 'a'];

    if (is_valid(counter)) return true;
    else {
        for (int i = 0; i < 26; ++i) {
            if (counter[i] > 0) {
                --counter[i];
                if (is_valid(counter)) return true;
                counter[i] += 2;
                if (is_valid(counter)) return true;
                --counter[i];
            }
        }
    }

    return false;
}


int main() {
    // Read input
    std::string str;
    std::cin >> str;

    std::cout << (solve(str) ? "YES" : "NO") << std::endl;

    return 0;
}

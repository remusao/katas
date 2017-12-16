#include <iostream>
#include <string>


bool is_palindrome(const std::string& str) {
    size_t size = str.size();
    for (int i = 0; i < (size / 2); ++i) {
        if (str[i] != str[size - i - 1]) return false;
    }
    return true;
}


int main() {
    bool found;
    int n;
    size_t size;
    std::string str;

    std::cin >> n;
    while (n--) {
        std::cin >> str;
        size = str.size();
        found = false;
        for (int i = 0; i < (size / 2); ++i) {
            if (str[i] != str[size - i - 1]) {
                // Try removing `i`th indice
                if (is_palindrome(str.substr(i + 1, size - 2 * i - 1))) {
                    std::cout << i << std::endl;
                    found = true;
                    break;
                }
                // Try removing `size - i - 1`th indice
                else if (is_palindrome(str.substr(i, size - 2 * i - 1))) {
                    std::cout << (size - i - 1) << std::endl;
                    found = true;
                    break;
                }
            }
        }
        if (!found) std::cout << -1 << std::endl;
    }
    return 0;
}

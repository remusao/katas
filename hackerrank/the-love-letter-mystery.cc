
#include <iostream>
#include <string>


int main() {
    int n;
    std::string str;

    std::cin >> n;
    while (n--) {
        std::cin >> str;
        size_t size = str.size();
        int result = 0;
        for (int i = 0; i < (size / 2); ++i) {
            if (str[i] != str[size - i - 1]) {
                result += abs(str[i] - str[size - i - 1]);
            }
        }
        std::cout << result << std::endl;
    }
    return 0;
}

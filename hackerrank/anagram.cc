#include <vector>
#include <string>
#include <iostream>


int main() {
    int n;
    std::cin >> n;

    std::string buffer;

    while (n--) {
        std::cin >> buffer;
        if (buffer.size() % 2 != 0) std::cout << -1 << std::endl;
        else {
            // Count occurrences
            std::vector<int> counter1(26, 0);
            std::vector<int> counter2(26, 0);

            for (int i = 0; i < (buffer.size() / 2); ++i) {
               counter1[buffer[i] - 'a']++;
            }
            for (int i = buffer.size() / 2; i < buffer.size(); ++i) {
               counter2[buffer[i] - 'a']++;
            }

            // Count differences
            int result = 0;
            for (int i = 0; i < 26; ++i) {
                result += abs(counter2[i] - counter1[i]);
            }
            std::cout << (result / 2) << std::endl;
        }

    }
    return 0;
}

#include <iostream>
#include <array>


int main() {
    std::string s1, s2;
    std::cin >> s1 >> s2;

    std::array<int, 26> c1, c2;
    c1.fill(0);
    c2.fill(0);

    for (char c: s1) ++c1[c - 'a'];
    for (char c: s2) ++c2[c - 'a'];

    int result = 0;
    for (int i = 0; i < 26; ++i) result += abs(c1[i] - c2[i]);

    std::cout << result << std::endl;

    return 0;
}

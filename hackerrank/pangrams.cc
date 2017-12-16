#include <iostream>
#include <string>
#include <vector>
#include <fstream>


int main()
{
    std::string str;
    std::getline(std::cin, str);

    std::vector<bool> mask(26, false);
    for (char c: str) {
        if (c >= 'A' && c <= 'Z') mask[c - 'A'] = true;
        else if (c >= 'a' && c <= 'z') mask[c - 'a'] = true;
    }

    bool pangram = true;
    for (bool b: mask) {
        pangram &= b;
    }

    if (pangram) std::cout << "pangram" << std::endl;
    else std::cout << "not pangram" << std::endl;

    return 0;
}

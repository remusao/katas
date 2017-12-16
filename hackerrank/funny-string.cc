#include <iostream>
#include <string>


int main()
{
    int n;
    bool funny;
    std::string str;

    std::cin >> n;
    while (n--) {
        std::cin >> str;
        size_t size = str.size();
        funny = true;

        for (int i = 1; i < size; ++i) {
            if (abs(str[i] - str[i - 1]) != abs(str[size - i - 1] - str[size - i])) {
                funny = false;
                break;
            }
        }
        if (funny) {
            std::cout << "Funny" << std::endl;
        }
        else {
            std::cout << "Not Funny" << std::endl;
        }
    }
    return 0;
}

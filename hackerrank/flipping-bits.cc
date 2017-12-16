#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>


int main() {
    int n;
    unsigned number;
    std::cin >> n;
    for (int i = 0; i < n; ++i)
    {
        std::cin >> number;
        std::cout << ~number << std::endl;
    }
    return 0;
}

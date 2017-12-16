#include <iostream>
#include <string>
#include <cmath>


int main() {
    std::string text;
    std::cin >> text;
    std::string clean;

    // Get size without spaces
    for (char c: text) {
        if (c >= 'a' && c <= 'z') clean += c;
    }
    int L = clean.size();

    int width = std::ceil(sqrt(L));
    int height = std::floor(sqrt(L));
    if (width * height < L) ++height;

    char array[height][width];
    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; ++j) {
            array[i][j] = '\0';
        }
    }
    int index = 0;

    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; ++j) {
            if (index < L) array[i][j] = clean[index++];
        }
    }

    index = 0;
    for (int i = 0; i < width; ++i) {
        for (int j = 0; j < height; ++j) {
            if (array[j][i] != '\0' && index < L) {
                std::cout << array[j][i];
                ++index;
            }
        }
        std::cout << ' ';
    }
    std::cout << std::endl;

    return 0;
}

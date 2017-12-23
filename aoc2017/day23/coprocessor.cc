
#include <iostream>
#include <cmath>

int main() {
    int r = 0;
    // b = 109300
    // c = 126300
    // b is incremented by 17 at every iteration
    for (int i = 109300; i <= 126300; i += 17) {
        if (i % 2 == 0 ||
            i % 3 == 0 ||
            i % 5 == 0 ||
            i % 7 == 0 ||
            i % 11 == 0 ||
            i % 13 == 0 ||
            i % 17 == 0
        ) {
            ++r;
            continue;
        }
        for (int j = 19, s = sqrt(i); j < s; j += 2) {
            if (i % j == 0) {
                ++r;
                break;
            }
        }
    }

    std::cout << r << std::endl;
}

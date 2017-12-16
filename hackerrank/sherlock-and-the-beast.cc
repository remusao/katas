#include <iostream>


void print_results(int fives, int threes) {
    if (fives == 0 && threes == 0) {
        std::cout << -1;
    }
    else {
        fives *= 3;
        threes *= 5;
        while (fives--) std::cout << '5';
        while (threes--) std::cout << '3';
    }
    std::cout << std::endl;
}


int main() {
    int t, n;
    std::cin >> t;

    while (t--) {
        std::cin >> n;

        int threes;
        int fives = n / 3;
        for (; fives >= 0; --fives) {
            if ((n - fives * 3) % 5 == 0) {
                threes = (n - fives * 3) / 5;
                break;
            }
        }

        if (fives >= 0 && threes >= 0) {
            print_results(fives, threes);
        }
        else std::cout << -1 << std::endl;
    }

    return 0;
}
